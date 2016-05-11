/*
 * Copyright 2038 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package calculators.results

import models._

case class BasicCalculator(annualAllowanceInPounds: Long) extends calculators.Calculator {
  calc => BasicCalculator

  def definedBenefit(implicit contribution: Contribution): Long = {
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    val year = contribution.taxPeriodStart.year
    if (year < 2015)
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
    else {
      if (year == 2015 && !contribution.isTriggered) {
        amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
      } else {
        amounts.definedBenefit.getOrElse(0L)
      }
    }
  }

  def definedContribution(implicit contribution: Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def annualAllowance(): Long = annualAllowanceInPounds*100L // convert allowance from pounds to pence

  def exceedingAllowance(implicit contribution: Contribution): Long = (calc.definedBenefit - calc.annualAllowance).max(0)

  def unusedAllowance(implicit contribution: Contribution): Long = (calc.annualAllowance - calc.definedBenefit).max(0)

  // total annual allowance possible
  // LN TODO Update to consider 2015 2 periods if this is reused for 2016
  def annualAllowanceCF(implicit previousPeriods:Seq[TaxYearResults]): Long = previousPeriods.map(_.summaryResult).slice(0,3).foldLeft(0L)(_+_.unusedAllowance) + calc.annualAllowance

  // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
  def annualAllowanceCCF(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    val unusedAllowances = previousPeriods.map(_.summaryResult).slice(0,2).foldLeft(0L)(_+_.unusedAllowance)

    if (contribution.taxPeriodStart.year < 2011 && calc.definedBenefit >= calc.annualAllowance) {
      (calc.annualAllowance + unusedAllowances - calc.definedBenefit.min(calc.annualAllowance)).max(0)
    } else if (calc.exceedingAllowance > 0) {
      val unusedAllowancesOf3rdPreviousYear = previousPeriods.map(_.summaryResult.unusedAllowance).slice(0,3).reverse.headOption.getOrElse(0L)
      if(unusedAllowancesOf3rdPreviousYear >= calc.exceedingAllowance)
        unusedAllowances
      else
        (unusedAllowances - (calc.exceedingAllowance - unusedAllowancesOf3rdPreviousYear)).max(0)
    } else {
      (calc.annualAllowance + unusedAllowances - calc.definedBenefit).max(0)
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = if (contribution.taxPeriodStart.year < 2011) -1 else (calc.definedBenefit - calc.annualAllowanceCF).max(0)

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    contribution.amounts.map {
      _ =>
      SummaryResult(calc.chargableAmount, 
                    calc.exceedingAllowance, 
                    calc.annualAllowance, 
                    calc.unusedAllowance, 
                    calc.annualAllowanceCF, 
                    calc.annualAllowanceCCF)
    }
  }
}

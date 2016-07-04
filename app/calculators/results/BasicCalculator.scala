/*
 * Copyright 2016 HM Revenue & Customs
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
import calculators.results.Utilities._

case class BasicCalculator(annualAllowanceInPounds: Long, previousPeriods:Seq[TaxYearResults], contribution: Contribution) {
  calc => BasicCalculator

  def definedBenefit(): Long = {
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

  def definedContribution(): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def annualAllowance(): Long = annualAllowanceInPounds*100L // convert allowance from pounds to pence

  def exceedingAllowance(): Long = (calc.definedBenefit - calc.annualAllowance).max(0)

  def unusedAllowance(): Long = (calc.annualAllowance - calc.definedBenefit).max(0)

  // total annual allowance possible
  // LN TODO Update to consider 2015 2 periods if this is reused for 2016
  def annualAllowanceCF(): Long = {
    previousPeriods.map(_.summaryResult.availableAAWithCCF).headOption.map(_ + calc.annualAllowance).getOrElse(calc.annualAllowance)
  }

  // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
  val previous3YearsActualUnused = basicActualUnused(this)(3)
  def annualAllowanceCCF(): Long = {
    val execeeding = calc.exceedingAllowance
    val previous3YearsUnused = previous3YearsActualUnused(previousPeriods,contribution)
    if (contribution.taxPeriodStart.year < 2011) {
      // Prior to 2011 nothing was liable for tax charge and carry forwards are allowed
      previous3YearsUnused
    } else {
      if (execeeding > 0) {
        val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())
        if (execeeding >= previousResults.availableAAWithCCF){
          0L
        } else {
          previous3YearsUnused
        }
      } else {
        previous3YearsUnused
      }
    }
  }

  def chargableAmount(): Long = {
    if (contribution.taxPeriodStart.year < 2011) - 1 else {
      (calc.definedBenefit - calc.annualAllowanceCF).max(0)
    }
  }

  def summary(): Option[Summary] = {
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

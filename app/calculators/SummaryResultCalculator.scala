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

package calculators

import models._
import calculators.results.Utilities._

class SummaryResultCalculator(annualAllowanceInPounds: Long, previousPeriods:Seq[TaxYearResults], contribution: Contribution) extends SummaryCalculator {

  def allowance(): Long = annualAllowanceInPounds
  
  protected lazy val _definedBenefit = {
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
  def definedBenefit(): Long = _definedBenefit

  protected lazy val _definedContribution = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
  def definedContribution(): Long = _definedContribution

  protected lazy val _aa = annualAllowanceInPounds*100L // convert allowance from pounds to pence
  def annualAllowance(): Long = _aa

  protected lazy val _exceedingAllowance = (definedBenefit - annualAllowance).max(0)
  def exceedingAllowance(): Long = _exceedingAllowance

  protected lazy val _unusedAllowance = (annualAllowance - definedBenefit).max(0)
  def unusedAllowance(): Long = _unusedAllowance

  // total annual allowance possible
  // LN TODO Update to consider 2015 2 periods if this is reused for 2016
  protected lazy val _annualAllowanceCF = previousPeriods.map(_.summaryResult.availableAAWithCCF).headOption.map(_ + annualAllowance).getOrElse(annualAllowance)
  def annualAllowanceCF(): Long = _annualAllowanceCF

  // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
  val previous3YearsActualUnused = basicActualUnused(this)(3)
  protected lazy val _annualAllowanceCCF = {
    val execeeding = exceedingAllowance
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
  def annualAllowanceCCF(): Long = _annualAllowanceCCF

  protected lazy val _chargableAmount = {
    if (contribution.taxPeriodStart.year < 2011) - 1 else {
      (definedBenefit - annualAllowanceCF).max(0)
    }
  }
  def chargableAmount(): Long = _chargableAmount

  def summary(): Option[Summary] = {
    contribution.amounts.map {
      _ =>
      SummaryResult(chargableAmount, 
                    exceedingAllowance, 
                    annualAllowance, 
                    unusedAllowance, 
                    annualAllowanceCF, 
                    annualAllowanceCCF)
    }
  }
}

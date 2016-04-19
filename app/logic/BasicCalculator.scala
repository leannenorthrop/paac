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

package logic

import models._

trait BasicCalculator extends Calculator {
  protected def getAnnualAllowanceInPounds: Long

  def summary(previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Option[SummaryResult] = {
    if (contribution.amounts.isDefined && (contribution.amounts.get.definedBenefit.isDefined || contribution.amounts.get.moneyPurchase.isDefined)) {
      val amounts = contribution.amounts.get
      var definedBenefit = if (amounts.definedBenefit.isDefined) amounts.definedBenefit.get else 0L
      val definedContribution = if (amounts.moneyPurchase.isDefined) amounts.moneyPurchase.get else 0L
      
      if (contribution.taxPeriodStart.year < 2015)
        definedBenefit += definedContribution

      if (isSupported(contribution) && definedBenefit >= 0) {
        // convert allowance from pounds to pence
        val annualAllowance: Long = getAnnualAllowanceInPounds*100L
        val exceedingAAAmount: Long = (definedBenefit - annualAllowance).max(0)
        val unusedAllowance: Long = (annualAllowance - definedBenefit).max(0)
        // total annual allowance possible
        val availableAAWithCF: Long = previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance) + annualAllowance
        // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
        val availableAAWithCCF: Long = if (contribution.taxPeriodStart.year < 2011 && definedBenefit >= annualAllowance) {
          (annualAllowance + previousPeriods.slice(0,2).foldLeft(0L)(_+_.unusedAllowance) - definedBenefit.min(annualAllowance)).max(0)
        } else {
          (annualAllowance + previousPeriods.slice(0,2).foldLeft(0L)(_+_.unusedAllowance) - definedBenefit).max(0)
        }
        val chargableAmount: Long = if (contribution.taxPeriodStart.year < 2011) -1 else (definedBenefit - availableAAWithCF).max(0)
        val unusedAllowanceCF: Long = 0L

        Some(SummaryResult(chargableAmount, exceedingAAAmount, annualAllowance, unusedAllowance, availableAAWithCF, availableAAWithCCF, unusedAllowanceCF))
      } else None
    } else None
  }
}
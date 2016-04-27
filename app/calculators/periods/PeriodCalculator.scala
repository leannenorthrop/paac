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

package calculators.periods

import models._

trait PeriodCalculator extends calculators.Calculator {
  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = previousPeriods.filter(_.input.taxPeriodStart.year < 2015).slice(0,3).foldLeft(0L)(_+_.summaryResult.unusedAllowance)

  def preFlexiSavings(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val values = previousPeriods.filter((r)=> r.input.isTriggered == false && (r.input.isPeriod1() || r.input.isPeriod2())).map {
      (row)=>
      val amounts = row.input.amounts.getOrElse(InputAmounts())
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
    }
    values.foldLeft(0L)( _ + _ )
  }

  def definedContribution(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
}

case class Group2Fields(chargableAmount: Long = 0,
                        exceedingAAAmount: Long = 0,
                        availableAllowance: Long = 0,
                        unusedAllowance: Long = 0,
                        availableAAWithCF: Long = 0,    // total available allowance for current year should be renamed to totalAA
                        availableAAWithCCF: Long = 0,   // available allowance carried forward to following year
                        unusedAllowanceCF: Long = 0,
                        moneyPurchaseAA: Long = 0,
                        alternativeAA: Long = 0,
                        dbist: Long = 0,
                        mpist: Long = 0,
                        alternativeChargableAmount: Long = 0,
                        defaultChargableAmount: Long = 0,
                        cumulativeMP: Long = 0,
                        cumulativeDB: Long = 0,
                        exceedingMPAA: Long = 0,
                        exceedingAAA: Long = 0,
                        unusedAA: Long = 0,
                        unusedMPAA: Long = 0,
                        preFlexiSavings: Long = 0,
                        postFlexiSavings: Long = 0) extends Summary
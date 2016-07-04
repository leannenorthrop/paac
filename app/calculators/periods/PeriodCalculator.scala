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
import calculators.periods.Utilities._
import calculators.results.Utilities._

trait PeriodCalculator {
  def definedContribution(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
  def basicCalculator(): calculators.results.BasicCalculator
  def definedBenefit(): Long
  def chargableAmount(): Long
  def exceedingAllowance(): Long
  def annualAllowance(): Long
  def unusedAllowance(): Long
  def aaCF(): Long
  def aaCCF(): Long
  def moneyPurchaseAA(): Long = 0L
  def alternativeAA(): Long = 0L
  def dbist(): Long = 0L
  def mpist(): Long = 0L
  def alternativeChargableAmount(): Long = 0L
  def defaultChargableAmount(): Long = 0L
  def cumulativeMP(): Long = 0L
  def cumulativeDB(): Long = 0L
  def exceedingMPAA(): Long = 0L
  def exceedingAAA(): Long = 0L
  def unusedAAA(): Long = 0L
  def unusedMPAA(): Long = 0L
  def preFlexiSavings(): Long = 0L
  def postFlexiSavings(): Long = 0L
  def isMPAAApplicable(): Boolean = false
  def acaCF() : Long = 0L
  def dcaCF() : Long = 0L

  val actualUnusedAllowance = actualUnusedFn(3)
  def previous3YearsUnusedAllowance(implicit previous:Seq[TaxYearResults]): Long = {
    val previousPeriods = previous.filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)
    previousPeriods.headOption.map {
      (row)=>
      val pensionPeriod = row.input.taxPeriodStart.copy(year=row.input.taxPeriodStart.year+1)
      val contribution = Contribution(pensionPeriod, pensionPeriod, Some(InputAmounts(0L,0L)))
      // unlike in actual unused method below we use simple basic extractor since period 1 and 2 are removed above and only dealing with years prior to 2015
      val actualUnusedLst = calculateActualUnused(toSummaryResultsTuple(basicCalculator))(previousPeriods, contribution).drop(1)
      actualUnusedAllowance(actualUnusedLst)
    }.getOrElse(0L)
  }

  def actualUnused(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[YearActualUnusedPair] = calculateActualUnused(extractValues(this))(previousPeriods, contribution)
}

object PeriodCalculator {
  def apply(allowanceInPounds: Long)(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): PeriodCalculator = {
    implicit val amountsCalculator = calculators.results.BasicCalculator(allowanceInPounds)
    if (contribution.isPeriod1) {
      Period1Calculator()
    } else if (contribution.isPeriod2) {
      Period2Calculator()
    } else {
      new PeriodCalculator() {
        def basicCalculator(): calculators.results.BasicCalculator = amountsCalculator
        def definedBenefit(): Long = amountsCalculator.definedBenefit
        def chargableAmount(): Long = amountsCalculator.chargableAmount
        def exceedingAllowance(): Long = amountsCalculator.exceedingAllowance
        def annualAllowance(): Long = amountsCalculator.annualAllowance
        def unusedAllowance(): Long = amountsCalculator.unusedAllowance
        def aaCF(): Long = amountsCalculator.annualAllowanceCF
        def aaCCF(): Long = amountsCalculator.annualAllowanceCCF
      }
    }
  }
}
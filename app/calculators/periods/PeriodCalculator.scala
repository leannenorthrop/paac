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

trait PeriodCalculator {
  def isBefore2015(taxYearResult: TaxYearResults): Boolean = taxYearResult.input.taxPeriodStart.year < 2015
  def pre2015Results(implicit previousPeriods:Seq[TaxYearResults]) = previousPeriods.filter(isBefore2015)
  def period1(implicit previousPeriods:Seq[TaxYearResults]) = previousPeriods.find(_.input.isPeriod1).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
  def previous(implicit previousPeriods:Seq[TaxYearResults]): Summary = previousPeriods.headOption.map(_.summaryResult).getOrElse(ExtendedSummaryFields())
  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = pre2015Results.slice(0,3).foldLeft(0L)(_+_.summaryResult.unusedAllowance)
  def previous2YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = pre2015Results.slice(0,2).foldLeft(0L)(_+_.summaryResult.unusedAllowance)

  def definedContribution(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
  
  def definedBenefit(): Long = 0L
  def chargableAmount(): Long = 0L
  def exceedingAllowance(): Long = 0L
  def annualAllowance(): Long = 0L
  def unusedAllowance(): Long = 0L
  def aaCF(): Long = 0L
  def aaCCF(): Long = 0L
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
}

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

  def isTriggered(implicit contribution: Contribution): Boolean = contribution.isTriggered

  def taxResultNotTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !tx.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)

  def taxResultTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !taxResultNotTriggered(tx)

  def maybeExtended(t: TaxYearResults): Option[ExtendedSummaryFields] = if (t.summaryResult.isInstanceOf[ExtendedSummaryFields]) Some(t.summaryResult.asInstanceOf[ExtendedSummaryFields]) else None

  def notTriggered(implicit previousPeriods:Seq[TaxYearResults]): Option[TaxYearResults] = previousPeriods.find(taxResultNotTriggered)

  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = notTriggered.flatMap(maybeExtended(_))

  def preTriggerInputs(implicit previousPeriods:Seq[TaxYearResults]): Option[Contribution] = notTriggered.map(_.input)

  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], c: Contribution): Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    // can't use period calculator's actual unused because of circular refeferences
    val contribution = Contribution(c.taxPeriodStart, c.taxPeriodEnd, Some(InputAmounts(0L,0L)))
    val pp = if (!previousPeriods.find(_.input.isPeriod1).isDefined) previousPeriods else previousPeriods.drop(1)
    basicCalculator().actualUnused(pp, contribution).drop(1).slice(0,3).foldLeft(0L)(_+_._2)
  }

  def actualUnused(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[(Int,Long)] = {
    type FlatValues = (Int, Long, Long, Long, Long)

    // based on basic calculator extract values
    def extractFlatValues(implicit p:Seq[TaxYearResults], contribution: Contribution): List[FlatValues] = {
      // handle period 1 and 2 separately so filter out of previous results
      val previousPeriods = p.filterNot(_.input.isTriggered).filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)

      // add back in either period 1 or 2 as the result for 2015
      val prefix = if (contribution.isPeriod1()) {
        List((2015, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      } else if (contribution.isPeriod2()) {
        List((2015, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      } else {
        List((contribution.taxPeriodStart.year, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      }

      // build list
      val list = (prefix ++
        previousPeriods.map {
          (result) =>
            val amounts = result.input.amounts.getOrElse(InputAmounts())
            val summary = result.summaryResult
            (result.input.taxPeriodStart.year, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
        }.toList).reverse

      // if period 2 or later there will be period 1 in the results so recalculate allowances when period 1 exceeds the allowance
      p.find(_.input.isPeriod1).map{
        (period1) =>
        val sr = period1.summaryResult
        if (sr.exceedingAAAmount > 0) {
          val l = list.filter(_._1<2015).reverse
          val newUnusedAllowances = basicCalculator.useAllowances(sr.exceedingAAAmount, 2015, 0, sr.unusedAllowance, l).drop(1).reverse
          val (before,after) = l.reverse.splitAt(4)
          val newAfter = newUnusedAllowances.zip(after).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
          before ++ newAfter ++ list.filter(_._1>2014)
        } else {
          list
        }
      }.getOrElse(list)
    }

    basicCalculator.calculate(extractFlatValues).map((tuple)=>(tuple._1, tuple._5))
  }
}

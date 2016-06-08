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

trait PensionAllowanceCalculator {

  protected def provideMissingYearContributions(contributions : Seq[Contribution]): List[Contribution] = {
    def sortByYearAndPeriod(left: Contribution, right: Contribution): Boolean = {
      if (left.taxPeriodStart.year == right.taxPeriodStart.year &&
          left.taxPeriodStart.year == 2015) {
        left.isPeriod1 && right.isPeriod2 || 
        (left.isPeriod1 && right.isPeriod1 && !left.amounts.get.triggered.get) ||
        (left.isPeriod2 && right.isPeriod2 && !left.amounts.get.triggered.get)
      } else {  
        left.taxPeriodStart.year < right.taxPeriodStart.year
      }
    }

    def generatePeriod1And2Contributions(inputsByTaxYear: Map[Int,Seq[Contribution]],
                                         lst:List[Contribution]): List[Contribution] = {
      def getPeriodContributions(): (List[Contribution], List[Contribution]) = {
        val contributions = inputsByTaxYear(2015).groupBy(_.isPeriod1)

        val p1 = contributions.get(true).map(_.toList).getOrElse(List(Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0)))))
        val p2 = contributions.get(false).map(_.toList).getOrElse(List(Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0)))))

        // whenever there is a triggered contribution then we must have 3 contributions over p1 and p2, one of which must be a pre-trigger contribution even if inputs are 0
        if (p1.size == 1 && p1(0).isTriggered) {
          val newPreTrigger = Contribution(PensionPeriod.PERIOD_1_2015_START, p1(0).taxPeriodStart, Some(InputAmounts(Some(0), Some(0), None, Some(false))))
          val newP1 = List(newPreTrigger) ++ p1
          (newP1, p2)
        } else if (p2.size == 1 && p2(0).isTriggered) {
          if (p1.size == 1 && !p1(0).isTriggered) {
            val newPreTrigger = Contribution(PensionPeriod.PERIOD_2_2015_START, p2(0).taxPeriodStart, Some(InputAmounts(Some(0), Some(0), None, Some(false))))
            val newP2 = List(newPreTrigger) ++ p2
            (newP2, p1)
          } else {
            (p1, p2)
          }
        } else {
          (p1, p2)
        }
      }

      val (p1Contributions, p2Contributions) = getPeriodContributions()
      (p1Contributions ++ p2Contributions ++ lst)
    }

    // Ensure sequential tax years have values converted none amounts to 0 for calculation purposes
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)
    val allContributions = ((PensionPeriod.EARLIEST_YEAR_SUPPORTED).min(inputsByTaxYear.keys.min) to inputsByTaxYear.keys.max).foldLeft(List[Contribution]()) {
      (lst:List[Contribution], year:Int) =>
        if (year != 2015) {
          val contribution = inputsByTaxYear.get(year).getOrElse(List(Contribution(year,0))).head
          (if (contribution.isEmpty) contribution.copy(amounts=Some(InputAmounts(0,0))) else contribution) :: lst
        } else { generatePeriod1And2Contributions(inputsByTaxYear, lst) }
    }
    allContributions.sortWith(sortByYearAndPeriod _)
  }

  def calculateAllowances(contributions : Seq[Contribution], doCollate: Boolean = false) : Seq[TaxYearResults] = {
    // Calculate results
    val inputsByTaxYear = contributions.groupBy(_.taxYearLabel)
    val results = provideMissingYearContributions(contributions).foldLeft(List[TaxYearResults]()) {
      (lst, contribution) =>

      val maybeCalculator = CalculatorFactory.get(contribution)
      val maybeSummary = maybeCalculator.map(_.summary(lst, contribution)).getOrElse(None)
      val summary: Summary = maybeSummary.getOrElse(SummaryResult())
      
      TaxYearResults(contribution, summary) :: lst
    }.dropWhile(_.input.taxYearLabel > inputsByTaxYear.keys.max).toList.reverse
    val v = results.dropWhile(_.input.taxYearLabel < inputsByTaxYear.keys.min).toList
    if (doCollate) {
      collate(v)
    } else {
      v
    }
  }

  def collate(calculationResults: Seq[TaxYearResults]): Seq[TaxYearResults] = {
    def r = calculationResults.toList
    def fetchTriggered(l:List[TaxYearResults]):Option[TaxYearResults] = l.find(_.input.isTriggered)
    def fetchNotTriggered(l:List[TaxYearResults]):Option[TaxYearResults] = l.find(!_.input.isTriggered)

    val triggerAmountRow = r.find(_.input.isTriggered)
    if (triggerAmountRow.isDefined) {
      val year2015ResultsMap = r.groupBy((r)=>r.input.isPeriod1||r.input.isPeriod2)(true).groupBy(_.input.isPeriod1)
      val period1Results = year2015ResultsMap.get(true).get
      val period2Results = year2015ResultsMap.get(false).get
      val non2015Results = r.filterNot((t)=>t.input.isPeriod1||t.input.isPeriod2)
      val results: List[TaxYearResults] = if (period1Results.size == 2) {
        val p1Triggered = fetchTriggered(period1Results).get
        val p1NotTriggered = fetchNotTriggered(period1Results).get
        val newP1 = p1Triggered.copy(input=p1NotTriggered.input)
        non2015Results ++ List(newP1) ++ List(fetchTriggered(period2Results).get)
      } else if (period2Results.size == 2) {
        val p2Triggered = fetchTriggered(period2Results).get
        val p2NotTriggered = fetchNotTriggered(period2Results).get
        val newP2 = p2Triggered.copy(input=p2NotTriggered.input)
        non2015Results ++ List(fetchNotTriggered(period1Results).get) ++ List(newP2)
      } else {
        r
      }
      results.toIndexedSeq
    } else {
      r.toIndexedSeq
    }
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

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

trait PensionAllowanceCalculator {

  protected def provideMissingYearContributions(contributions : Seq[Contribution]): List[Contribution] = {
    def sortByYearAndPeriod(left: Contribution, right: Contribution): Boolean = {
      if (left.taxPeriodStart.year == right.taxPeriodStart.year &&
          left.taxPeriodStart.year == 2015) {
        left.isPeriod1 && right.isPeriod2
      } else {  
        left.taxPeriodStart.year < right.taxPeriodStart.year
      }
    }

    def generatePeriod1And2Contributions(inputsByTaxYear: Map[Int,Seq[Contribution]],
                                         lst:List[Contribution]): List[Contribution] = {
      def getPeriodContributions(): (List[Contribution], List[Contribution]) = {
        val contributions = inputsByTaxYear(2015).groupBy(_.isPeriod1)

        (contributions.get(true).map(_.toList).getOrElse(List(Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0))))),
         contributions.get(false).map(_.toList).getOrElse(List(Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0))))))
      }

      val (p1Contributions, p2Contributions) = getPeriodContributions()
      (p1Contributions ++ p2Contributions ++ lst)
    }

    // Ensure sequential tax years have values converted none amounts to 0 for calculation purposes
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)
    val allContributions = ((2006).min(inputsByTaxYear.keys.min) to inputsByTaxYear.keys.max).foldLeft(List[Contribution]()) {
      (lst:List[Contribution], year:Int) =>
        if (year != 2015) {
          val contribution = inputsByTaxYear.get(year).getOrElse(List(Contribution(year,0))).head
          (if (contribution.isEmpty) contribution.copy(amounts=Some(InputAmounts(0,0))) else contribution) :: lst
        } else { generatePeriod1And2Contributions(inputsByTaxYear, lst) }
    }

    allContributions.sortWith(sortByYearAndPeriod _)
  }

  def calculateAllowances(contributions : Seq[Contribution]) : Seq[TaxYearResults] = {
    // Calculate results
    val inputsByTaxYear = contributions.groupBy(_.taxYearLabel)
    val results = provideMissingYearContributions(contributions).foldLeft(List[TaxYearResults]()) {
      (lst, contribution) =>

      val calculator = CalculatorFactory.get(contribution)
      val maybeSummary = calculator.map(_.summary(lst.map(_.summaryResult), contribution).getOrElse(SummaryResult()))
      val summary: SummaryResult = maybeSummary.getOrElse(SummaryResult())
      
      TaxYearResults(contribution, summary) :: lst
    }.dropWhile(_.input.taxYearLabel > inputsByTaxYear.keys.max).toList.reverse
    val v = results.dropWhile(_.input.taxYearLabel < inputsByTaxYear.keys.min).toList
    v
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

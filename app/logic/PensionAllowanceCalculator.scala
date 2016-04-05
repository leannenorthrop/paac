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

  def calculateAllowances(contributions : Seq[Contribution]) : Seq[TaxYearResults] = {
    // Ensure sequential tax years have values converted none amounts to 0 for calculation purposes
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)
    val allContributions = (inputsByTaxYear.keys.min to inputsByTaxYear.keys.max).foldLeft(List[Contribution]()) {
      (lst:List[Contribution], year:Int) =>
      // TODO implement support partial tax years
      val contribution = inputsByTaxYear.get(year).getOrElse(List(Contribution(year,0))).head
      (if (contribution.isEmpty) contribution.copy(amounts=Some(InputAmounts(0,0))) else contribution) :: lst
    }.sortWith(_.taxPeriodStart.year < _.taxPeriodStart.year)

    // Calculate results
    allContributions.foldLeft(List[TaxYearResults]()) {
      (lst, contribution) =>

      val factory = CalculatorFactory.get(contribution)
      val maybeSummary = factory.map(_.summary(lst.map(_.summaryResult), contribution).getOrElse(SummaryResult()))
      val summary: SummaryResult = maybeSummary.getOrElse(SummaryResult())
      
      TaxYearResults(contribution, summary) :: lst
    }.reverse
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

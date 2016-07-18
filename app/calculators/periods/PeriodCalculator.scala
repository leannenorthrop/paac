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
import calculators.ExtendedSummaryCalculator
import calculators.SummaryResultCalculator
import calculators.periods.Utilities._
import calculators.results.Utilities._
import calculators.Utilities._

trait PeriodCalculator extends ExtendedSummaryCalculator {
  def previous3YearsUnusedAllowance(implicit previous:Seq[TaxYearResults]): Long = {
    val previousPeriods = previous.filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)
    previousPeriods.headOption.map {
      (row)=>
      val pensionPeriod = row.input.taxPeriodStart.copy(year=row.input.taxPeriodStart.year+1)
      val contribution = Contribution(pensionPeriod, pensionPeriod, Some(InputAmounts(0L,0L)))
      // use simple basic extractor since period 1 and 2 are removed above and only dealing with years prior to 2015
      val actualUnusedLst = actualUnusedAllowancesFn(extractor(new SummaryResultCalculator(allowance, previousPeriods, contribution)))(previousPeriods, contribution).drop(1)
      actualUnusedFn(3)(actualUnusedLst)
    }.getOrElse(0L)
  }
}

object PeriodCalculator {
  def apply(allowanceInPounds: Long)(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): PeriodCalculator = {
    implicit val annualAllowanceInPounds = allowanceInPounds
    if (contribution.isPeriod1) {
      new Period1Calculator
    } else if (contribution.isPeriod2) {
      new Period2Calculator
    } else {
      new SummaryResultCalculator(annualAllowanceInPounds, previousPeriods, contribution) with PeriodCalculator
    }
  }
}
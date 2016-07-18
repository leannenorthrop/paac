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

package calculators.results

import models._
import calculators._
import calculators.Utilities._
import calculators.periods.PeriodCalculator

package object Utilities {
  /**
    Extractor to convert list of tax year results and 'current' contribution into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
    To be removed once 2016 calculator is implemented.
  */
  val extractor: SummaryCalculator => ToTupleFn = calc => {
    (p, c) => {
      implicit val previousPeriods = p
      implicit val contribution = c
      implicit val calculator = PeriodCalculator(calc.allowance)
      (List[SummaryResultsTuple](c) ++ List[SummaryResultsTuple](previousPeriods:_*)).reverse
    }
  }

  /**
    Helper function to calculate actual unused from 'current' contribution and previous results.
    This is here because the summary results only ever hold the unused allowance for the particular year in question
    but subsequent years can reduce the unused allowance further by exceeding the 'annual' allowance
  */
  def basicActualUnused(calculator: SummaryCalculator): Int => (Seq[TaxYearResults], Contribution) => Long = years => (p,c) => actualUnused(extractor(calculator))(years)(p,c)
}

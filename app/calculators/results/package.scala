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
  val extractor: SummaryCalculator => ToTupleFn = calc => {
    (p, c) => {
      implicit val previousPeriods = p
      implicit val contribution = c
      implicit val calculator = PeriodCalculator(calc.allowance)
      (List[SummaryResultsTuple](c) ++ List[SummaryResultsTuple](previousPeriods:_*)).reverse
    }
  }

  def basicActualUnused(calculator: SummaryCalculator): Int => (Seq[TaxYearResults], Contribution) => Long = years => (p,c) => actualUnused(extractor(calculator))(years)(p,c)
}

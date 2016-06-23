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

trait PeriodCalculatorFactory {
  def get(allowanceInPounds: Long)(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution) : Option[PeriodCalculator] = {
    implicit val amountsCalculator = calculators.results.BasicCalculator(allowanceInPounds)
    if (contribution.isPeriod1) {
      Some(Period1Calculator())
    } else if (contribution.isPeriod2) {
      Some(Group2P2Calculator())
      //if (contribution.isGroup3) Some(Period2Calculator())
      //else Some(Group2P2Calculator())
    } else {
      None
    }
  }
}

object PeriodCalculatorFactory extends PeriodCalculatorFactory {
}

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
      if (contribution.isGroup3 || contribution.isGroup2) Some(Group2P1Calculator())
      else Some(Group1P1Calculator())
    } else if (contribution.isPeriod2) {
      if (contribution.isGroup3) Some(Group3P2Calculator())
      else if (contribution.isGroup2) Some(Group2P2Calculator())
      else Some(Group1P2Calculator())
    } else {
      None
    }
  }
}

object PeriodCalculatorFactory extends PeriodCalculatorFactory {
}

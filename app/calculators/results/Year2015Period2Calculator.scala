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

import config.PaacConfiguration
import models._
import calculators.periods._ 

object Year2015Period2Calculator extends calculators.AllowanceCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period2Calculator")).getOrElse(0L)

  def isSupported(contribution:Contribution): Boolean = contribution.isPeriod2() && !contribution.isEmpty

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    if (isSupported(contribution)) {
      val amountsCalculator = BasicCalculator(getAnnualAllowanceInPounds)
      if (contribution.isGroup1()) {
        Group1P2Calculator(amountsCalculator).summary
      } else if (contribution.isGroup2) {
        Group2P2Calculator(amountsCalculator).summary
      } else None
    } else None
  }
}

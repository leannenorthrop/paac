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

trait BasicAllowanceCalculator extends calculators.AllowanceCalculator {
  protected def getAnnualAllowanceInPounds: Long

  def allowance(): Long = getAnnualAllowanceInPounds * 100L

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    if (contribution.amounts.isDefined && !contribution.isEmpty) {
      val amountsCalculator = BasicCalculator(getAnnualAllowanceInPounds)
      var definedBenefit = amountsCalculator.definedBenefit
      if (isSupported(contribution) && definedBenefit >= 0) {
        amountsCalculator.summary
      } else None
    } else None
  }
}

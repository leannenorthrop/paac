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

import config.PaacConfiguration
import models._

object Year2015Period1Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period1Calculator")).getOrElse(80000L)

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod1()
  }

  override def summary(previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Option[SummaryResult] = {
    // Period 1 only allows maximum carry forward of 40k (here in pence values)
    super.summary(previousPeriods, contribution).map {
      (results) =>
      if (results.unusedAllowance > 4000000L) {
          val ua = if (results.unusedAllowance > 4000000L) 4000000L else results.unusedAllowance
          val accf = if (results.availableAAWithCCF > 4000000L) results.availableAAWithCCF - 4000000L else results.availableAAWithCCF
          results.copy(unusedAllowance=ua,
                       availableAAWithCCF=accf)
        } else {
          results
        }
    }
  }
}

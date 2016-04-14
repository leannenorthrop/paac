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
          val annualAllowance: Long = getAnnualAllowanceInPounds*100L
          val amounts = contribution.amounts.get
          var definedBenefit = if (amounts.definedBenefit.isDefined) amounts.definedBenefit.get else 0L
          val exceedingAAAmount: Long = (definedBenefit - annualAllowance).max(0)

          // Key to understanding period 1 is that only a maximum of 40k is carried over to period 2 
          // period 2 relies on this for it's allowance as there is no annual allowance for period 2
          val ua = results.unusedAllowance.min(4000000L)

          // cumulative carry forwards is 3 previous years plus current year's maximum carry forwards
          val availableAAWithCCF: Long = (previousPeriods.slice(0,3).foldLeft(4000000L)(_+_.unusedAllowance) - (definedBenefit-exceedingAAAmount)).max(0)
          val availableAAWithCF: Long = annualAllowance + previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)
          results.copy(unusedAllowance=ua,availableAAWithCCF=availableAAWithCCF,availableAAWithCF=availableAAWithCF)
        } else {
          results
        }
    }
  }
}

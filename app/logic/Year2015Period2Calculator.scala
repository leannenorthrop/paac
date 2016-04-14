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

object Year2015Period2Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period2Calculator")).getOrElse(0L)

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod2()
  }

  override def summary(previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Option[SummaryResult] = {
    // Period 1 only allows maximum carry forward of 40k (here in pence values)
    super.summary(previousPeriods, contribution).map {
      (results) =>
      val amounts = contribution.amounts.get
      var definedBenefit = amounts.definedBenefit.getOrElse(0L)

      // key to understanding period 2 is that there is no allowance 
      // only unused carried forward allowance up to maximum of 40K from period 1
      // summed with previous 3 years
      val period1UnusedAllowance: Long = previousPeriods.headOption.map(_.unusedAllowance).getOrElse(0L)
      val previous3YearsUnusedAllowances: Long = previousPeriods.drop(1).slice(0,3).foldLeft(0L)(_+_.unusedAllowance) // drop 1 to ignore period 1 and get only 3 previous years

      // Unused and exceeding
      val unusedAllowance: Long = (period1UnusedAllowance - definedBenefit).max(0)
      val exceedingAAAmount: Long = (definedBenefit - (previous3YearsUnusedAllowances + period1UnusedAllowance)).max(0)

      // Carry forwards for allowances
      val availableAAWithCF: Long = previousPeriods.drop(1).slice(0,3).foldLeft(if (period1UnusedAllowance == 0) 4000000L else period1UnusedAllowance)(_+_.unusedAllowance)
      val availableAAWithCCF: Long = (previousPeriods.drop(1).slice(0,2).foldLeft(period1UnusedAllowance)(_+_.unusedAllowance) - (definedBenefit-exceedingAAAmount)).max(0)
      
      val chargableAmount: Long = (definedBenefit - (previous3YearsUnusedAllowances + period1UnusedAllowance)).max(0)

      results.copy(availableAAWithCF = availableAAWithCF,    // total available allowance for current year
                   availableAAWithCCF = availableAAWithCCF,  // available allowance carried forward to following year
                   unusedAllowance = unusedAllowance,
                   chargableAmount = chargableAmount,
                   exceedingAAAmount = exceedingAAAmount)
    }
  }
}

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

case class Group1P1Calculator(amountsCalculator: BasicAmountsCalculator) {
  me => Group1P2Calculator

  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def aaCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = amountsCalculator.annualAllowance + previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def aaCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val annualAllowance = amountsCalculator.annualAllowance
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance()
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (amountsCalculator.unusedAllowance.min(4000000L) + previous3YearsUnusedAllowance).max(0)
    }
  }

  def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    Some(SummaryResult(amountsCalculator.chargableAmount, 
                       amountsCalculator.exceedingAllowance, 
                       amountsCalculator.annualAllowance, 
                       amountsCalculator.unusedAllowance.min(4000000L), 
                       me.aaCF, 
                       me.aaCCF, 
                       0L))
  }
}

object Year2015Period1Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period1Calculator")).getOrElse(80000L)
  protected val amountsCalculator: BasicAmountsCalculator = BasicAmountsCalculator(getAnnualAllowanceInPounds)
  protected val group1Calculator = Group1P1Calculator(amountsCalculator)
  //protected val group2Calculator = Group2P1Calculator(amountsCalculator)

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod1()
  }

  override def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    if (isSupported(contribution)) {
      if (contribution.isGroup1()) {
        // Period 1 only allows maximum carry forward of 40k (here in pence values)
        group1Calculator.summary
      } else if (contribution.isGroup2) {
        None
      } else None
    } else None
  }
}

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

case class Group1P2Calculator(amountsCalculator: BasicAmountsCalculator) {
  me => Group1P2Calculator
  def period1UnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Long = previousPeriods.headOption.map(_.unusedAllowance).getOrElse(0L)

  def previous3YearsUnusedAllowances(implicit previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Long = previousPeriods.drop(1).slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def previous2YearsUnusedAllowances(implicit previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Long = previousPeriods.drop(1).slice(0,2).foldLeft(0L)(_+_.unusedAllowance) 

  def unusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = (me.period1UnusedAllowance - amountsCalculator.definedBenefit).max(0)

  def exceedingAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = (amountsCalculator.definedBenefit - me.period1UnusedAllowance).max(0)

  def annualAllowanceCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = if (previousPeriods.headOption.isDefined) previousPeriods.head.availableAAWithCCF else 0L

  def annualAllowanceCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = if (amountsCalculator.definedBenefit == 0) ((me.previous2YearsUnusedAllowances+me.period1UnusedAllowance) - amountsCalculator.definedBenefit).max(0) 
   else (me.annualAllowanceCF - amountsCalculator.definedBenefit).max(0)

  def chargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
      val cf = if (previousPeriods.headOption.isDefined) previousPeriods.head.availableAAWithCCF else 0L
      if (amountsCalculator.definedBenefit > cf) amountsCalculator.definedBenefit - cf else 0L
  }
}

object Year2015Period2Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period2Calculator")).getOrElse(0L)
  protected val group1Calculator = Group1P2Calculator(BasicAmountsCalculator(getAnnualAllowanceInPounds))

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod2()
  }

  override def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = if (isSupported(contribution) && contribution.isGroup1()) {
      // Period 1 only allows maximum carry forward of 40k (here in pence values)
      super.summary(previousPeriods, contribution).map {
        (results) =>
        results.copy(availableAAWithCF = group1Calculator.annualAllowanceCF,    // total available allowance for current year
                     availableAAWithCCF = group1Calculator.annualAllowanceCCF,  // available allowance carried forward to following year
                     unusedAllowance = group1Calculator.unusedAllowance,
                     chargableAmount = group1Calculator.chargableAmount,
                     exceedingAAAmount = group1Calculator.exceedingAllowance)
      }
    } else None
}

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
  def period1UnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.headOption.map(_.unusedAllowance).getOrElse(0L)

  def previous2YearsUnusedAllowances(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.drop(1).slice(0,2).foldLeft(0L)(_+_.unusedAllowance) 

  def unusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = (me.period1UnusedAllowance - amountsCalculator.definedBenefit).max(0)

  def exceedingAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = (amountsCalculator.definedBenefit - me.period1UnusedAllowance).max(0)

  def annualAllowanceCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = if (previousPeriods.headOption.isDefined) previousPeriods.head.availableAAWithCCF else 0L

  def annualAllowanceCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val previous2YearsUnusedAllowances = me.previous2YearsUnusedAllowances
    val period1UnusedAllowance = me.period1UnusedAllowance
    if (definedBenefit == 0) 
      previous2YearsUnusedAllowances + period1UnusedAllowance 
    else if (definedBenefit > period1UnusedAllowance &&
            previousPeriods.headOption.map(_.exceedingAAAmount).getOrElse(0L) == 0 &&
            previousPeriods.slice(0,3).exists(_.unusedAllowance == 0 && me.chargableAmount == 0)) {
      previous2YearsUnusedAllowances 
    } else if (definedBenefit < period1UnusedAllowance &&
               previousPeriods.headOption.map(_.exceedingAAAmount).getOrElse(0L) == 0) {
      ((previous2YearsUnusedAllowances + period1UnusedAllowance) - definedBenefit).max(0)
    } else {
      (me.annualAllowanceCF - definedBenefit).max(0)
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val cf = previousPeriods.headOption.map(_.availableAAWithCCF).getOrElse(0L)
    if (definedBenefit > cf) definedBenefit - cf else 0L
  }

  def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    amountsCalculator.summary(previousPeriods, contribution).map((results)=>results.copy(availableAAWithCF = me.annualAllowanceCF,    // total available allowance for current year
                                                            availableAAWithCCF = me.annualAllowanceCCF,  // available allowance carried forward to following year
                                                            unusedAllowance = me.unusedAllowance,
                                                            chargableAmount = me.chargableAmount,
                                                            exceedingAAAmount = me.exceedingAllowance))
  }
}

object Year2015Period2Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period2Calculator")).getOrElse(0L)

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod2() && !contribution.isEmpty
  }

  override def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = if (isSupported(contribution)) {
      if (contribution.isGroup1()) 
        Group1P2Calculator(BasicAmountsCalculator(getAnnualAllowanceInPounds)).summary
      else None
    } else None
}

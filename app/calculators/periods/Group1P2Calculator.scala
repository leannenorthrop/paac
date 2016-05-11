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
import calculators.results.BasicCalculator

case class Group1P2Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me => Group1P2Calculator

  def period1UnusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = previousPeriods.headOption.map(_.summaryResult.unusedAllowance).getOrElse(0L)
  def previous2YearsUnusedAllowances(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = previousPeriods.drop(1).slice(0,2).foldLeft(0L)(_+_.summaryResult.unusedAllowance) 
  def unusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = (me.period1UnusedAllowance - amountsCalculator.definedBenefit).max(0)
  def exceedingAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = (amountsCalculator.definedBenefit - me.period1UnusedAllowance).max(0)
  def annualAllowanceCF(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = if (previousPeriods.headOption.isDefined) previousPeriods.head.summaryResult.availableAAWithCCF else 0L

  def annualAllowanceCCF(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val previous2YearsUnusedAllowances = me.previous2YearsUnusedAllowances
    val period1UnusedAllowance = me.period1UnusedAllowance
    if (definedBenefit == 0)
      previous2YearsUnusedAllowances + period1UnusedAllowance 
    else if (definedBenefit > period1UnusedAllowance &&
            previousPeriods.headOption.map(_.summaryResult.exceedingAAAmount).getOrElse(0L) == 0 &&
            previousPeriods.slice(0,3).exists(_.summaryResult.unusedAllowance == 0 && me.chargableAmount == 0)) {
      previous2YearsUnusedAllowances 
    } else if (definedBenefit < period1UnusedAllowance &&
               previousPeriods.headOption.map(_.summaryResult.exceedingAAAmount).getOrElse(0L) == 0) {
      ((previous2YearsUnusedAllowances + period1UnusedAllowance) - definedBenefit).max(0)
    } else {
      (me.annualAllowanceCF - definedBenefit).max(0)
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val cf = previousPeriods.headOption.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)
    if (definedBenefit > cf) definedBenefit - cf else 0L
  }

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    amountsCalculator.summary(previousPeriods, contribution).map{
      (results)=>
      SummaryResult(me.chargableAmount,
                    me.exceedingAllowance,
                    results.availableAllowance,
                    me.unusedAllowance,
                    me.annualAllowanceCF,    // total available allowance for current year should be renamed to totalAA
                    me.annualAllowanceCCF,   // available allowance carried forward to following year
                    0L)
    }
  }
}

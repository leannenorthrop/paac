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

case class Group1P2Calculator(implicit amountsCalculator: BasicCalculator, 
                                       previousPeriods:Seq[TaxYearResults], 
                                       contribution: Contribution) extends PeriodCalculator {

  def period1UnusedAllowance(): Long = previousPeriods.headOption.map(_.summaryResult.unusedAllowance).getOrElse(0L)
  
  override def annualAllowance(): Long = amountsCalculator.annualAllowance
  
  override def unusedAllowance(): Long = (period1UnusedAllowance - amountsCalculator.definedBenefit).max(0)
  
  override def exceedingAllowance(): Long = (amountsCalculator.definedBenefit - period1UnusedAllowance).max(0)
  
  override def aaCF(): Long = if (previousPeriods.headOption.isDefined) previousPeriods.head.summaryResult.availableAAWithCCF else 0L

  override def aaCCF(): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    if (definedBenefit == 0)
      previous2YearsUnusedAllowance + period1UnusedAllowance 
    else if (definedBenefit > period1UnusedAllowance &&
            previousPeriods.headOption.map(_.summaryResult.exceedingAAAmount).getOrElse(0L) == 0 &&
            previousPeriods.slice(0,3).exists(_.summaryResult.unusedAllowance == 0 && chargableAmount == 0)) {
      previous2YearsUnusedAllowance 
    } else if (definedBenefit < period1UnusedAllowance &&
               previousPeriods.headOption.map(_.summaryResult.exceedingAAAmount).getOrElse(0L) == 0) {
      ((previous2YearsUnusedAllowance + period1UnusedAllowance) - definedBenefit).max(0)
    } else {
      (aaCF - definedBenefit).max(0)
    }
  }

  override def chargableAmount(): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val cf = previousPeriods.headOption.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)
    if (definedBenefit > cf) definedBenefit - cf else 0L
  }
}

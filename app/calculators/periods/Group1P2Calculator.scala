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

  def noCharge(): Boolean = previousPeriods.slice(0,3).exists(_.summaryResult.unusedAllowance == 0 && chargableAmount == 0)

  override def definedBenefit(): Long = amountsCalculator.definedBenefit

  override def annualAllowance(): Long = amountsCalculator.annualAllowance
  
  override def unusedAllowance(): Long = (period1.unusedAllowance - definedBenefit).max(0)
  
  override def exceedingAllowance(): Long = (definedBenefit - period1.unusedAllowance).max(0)
  
  override def aaCF(): Long = previousResults.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  override def aaCCF(): Long = {
    if (definedBenefit == 0) {
      val year2014 = pre2015Results.map(_.summaryResult).headOption.getOrElse(SummaryResult()).availableAAWithCCF
      val aaccf = previousResults.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)
      if (year2014 == 0) {
        aaccf
      } else {
        val oldestYearAvailableAllowance = previousPeriods.map(_.summaryResult).slice(0,4).reverse.headOption.getOrElse(SummaryResult()).unusedAllowance
        (aaccf - oldestYearAvailableAllowance).max(0)
      }
    } else if (definedBenefit > period1.unusedAllowance && previous.exceedingAAAmount == 0 && noCharge) {
      previous2YearsUnusedAllowance 
    } else if (definedBenefit < period1.unusedAllowance && previous.exceedingAAAmount == 0) {
      ((previous2YearsUnusedAllowance + period1.unusedAllowance) - definedBenefit).max(0)
    } else {
      (aaCF - definedBenefit).max(0)
    }
  }

  override def chargableAmount(): Long = {
    val cf = previous.availableAAWithCCF
    if (definedBenefit > cf) definedBenefit - cf else 0L
  }
}

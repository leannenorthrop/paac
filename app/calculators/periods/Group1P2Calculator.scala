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
  def basicCalculator(): BasicCalculator = amountsCalculator
  
  def noCharge(): Boolean = previousPeriods.slice(0,3).exists(_.summaryResult.unusedAllowance == 0 && chargableAmount == 0)

  override def definedBenefit(): Long = amountsCalculator.definedBenefit

  override def annualAllowance(): Long = amountsCalculator.annualAllowance
  
  override def unusedAllowance(): Long = (period1.unusedAllowance - definedBenefit).max(0)
  
  override def exceedingAllowance(): Long = (definedBenefit - period1.unusedAllowance).max(0)
  
  override def aaCF(): Long = previousResults.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  override def aaCCF(): Long = {
    val execeeding = exceedingAllowance

    if (execeeding > 0) {
      val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())

      val period1AACCF = previousResults.availableAAWithCCF
      if (execeeding >= period1AACCF) {
        0L
      } else {
        val unusedAllowanceList = actualUnused.slice(0, 3).map(_._2)
        unusedAllowanceList.foldLeft(0L)(_ + _)
      }
    } else {
      val unusedAllowanceList = actualUnused.slice(0, 3).map(_._2)
      unusedAllowanceList.foldLeft(0L)(_ + _)
    }

  }

  override def chargableAmount(): Long = {
    val cf = previous.availableAAWithCCF
    if (definedBenefit > cf) definedBenefit - cf else 0L
  }
}

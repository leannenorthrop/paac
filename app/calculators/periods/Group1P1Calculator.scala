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

case class Group1P1Calculator(implicit amountsCalculator: BasicCalculator,
                                       previousPeriods:Seq[TaxYearResults], 
                                       contribution: Contribution) extends PeriodCalculator {
  def basicCalculator(): BasicCalculator = amountsCalculator
  
  override def chargableAmount(): Long = basicCalculator().chargableAmount

  override def exceedingAllowance(): Long = basicCalculator().exceedingAllowance

  override def annualAllowance(): Long = basicCalculator().annualAllowance

  override def unusedAllowance(): Long = basicCalculator().unusedAllowance.min(4000000L)

  override def aaCF(): Long = annualAllowance + previousResults.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  override def definedBenefit(): Long = basicCalculator().definedBenefit

  override def aaCCF(): Long = {
    val execeeding = exceedingAllowance

    if (execeeding > 0) {
      val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())
      val prePeriod1AACCF = previousResults.availableAAWithCCF

      if (execeeding >= prePeriod1AACCF) {
        0L
      } else {
        val unusedAllowanceList = actualUnused.slice(0, 4).map(_._2)
        unusedAllowanceList.foldLeft(0L)(_ + _)
      }

    } else {
      val unusedAllowanceList = actualUnused.slice(0, 4).map(_._2)
      unusedAllowanceList.foldLeft(0L)(_ + _)
    }
  }
}

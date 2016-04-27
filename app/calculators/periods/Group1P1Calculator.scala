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

case class Group1P1Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me => Group1P1Calculator

  def aaCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = amountsCalculator.annualAllowance + me.previous3YearsUnusedAllowance()

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
                       me.aaCCF))
  }
}
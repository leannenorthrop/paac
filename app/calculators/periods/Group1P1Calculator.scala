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
  override def chargableAmount(): Long = amountsCalculator.chargableAmount

  override def exceedingAllowance(): Long = amountsCalculator.exceedingAllowance

  override def annualAllowance(): Long = amountsCalculator.annualAllowance

  override def unusedAllowance(): Long = amountsCalculator.unusedAllowance.min(4000000L)

  override def aaCF(): Long = amountsCalculator.annualAllowance + previous3YearsUnusedAllowance

  override def definedBenefit(): Long = amountsCalculator.definedBenefit

  override def aaCCF(): Long = {
    if (definedBenefit >= amountsCalculator.annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (unusedAllowance + previous3YearsUnusedAllowance).max(0)
    }
  }
}

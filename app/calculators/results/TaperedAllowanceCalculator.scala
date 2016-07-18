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

package calculators.results

import calculators._
import models._

case class TaperedAllowanceCalculator(implicit allowanceInPounds: Long,
                                               previousPeriods:Seq[TaxYearResults], 
                                               contribution:Contribution) extends ExtendedSummaryCalculator {

  def allowance(): Long = allowanceInPounds

  def definedBenefit(): Long = 0L

  def definedContribution(): Long = 0L

  def annualAllowance(): Long = 0L

  def exceedingAllowance(): Long = 0L

  def unusedAllowance(): Long = 0L

  def annualAllowanceCF(): Long = 0L

  def annualAllowanceCCF(): Long = 0L

  def chargableAmount(): Long = 0L
}

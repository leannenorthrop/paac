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

package calculators

import models._
import calculators.results._

trait Calculator {
  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Option[Summary]
}

trait AllowanceCalculator extends Calculator {
  def allowance(): Long
  def isSupported(contribution:Contribution):Boolean
}

trait SummaryCalculator {
  def allowance(): Long
  def definedBenefit(): Long 
  def definedContribution(): Long
  def annualAllowance(): Long
  def exceedingAllowance(): Long
  def unusedAllowance(): Long
  def annualAllowanceCF(): Long
  def annualAllowanceCCF(): Long
  def chargableAmount(): Long
}

trait ExtendedSummaryCalculator extends SummaryCalculator {
  def moneyPurchaseAA(): Long = 0L
  def alternativeAA(): Long = 0L
  def dbist(): Long = 0L
  def mpist(): Long = 0L
  def alternativeChargableAmount(): Long = 0L
  def defaultChargableAmount(): Long = 0L
  def cumulativeMP(): Long = 0L
  def cumulativeDB(): Long = 0L
  def exceedingMPAA(): Long = 0L
  def exceedingAAA(): Long = 0L
  def unusedAAA(): Long = 0L
  def unusedMPAA(): Long = 0L
  def preFlexiSavings(): Long = 0L
  def postFlexiSavings(): Long = 0L
  def isMPAAApplicable(): Boolean = false
  def acaCF() : Long = 0L
  def dcaCF() : Long = 0L
}

trait CalculatorFactory {
  protected val calculators : List[AllowanceCalculator]
  protected def get(contribution:Contribution) : Option[AllowanceCalculator] = calculators.find(_.isSupported(contribution))
}

object Calculator extends CalculatorFactory {
  protected override val calculators : List[AllowanceCalculator] = List(Pre2014Calculator, Year2014Calculator, Year2015Period1Calculator, Year2015Period2Calculator, Post2015Calculator)
  
  def apply(contribution:Contribution): AllowanceCalculator = {
    get(contribution).getOrElse {
      new AllowanceCalculator() {
        def summary(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Option[Summary] = None
        def allowance(): Long = 0L
        def isSupported(contribution:Contribution):Boolean = false
      }
    }
  }
}

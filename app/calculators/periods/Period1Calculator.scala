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

case class Period1Calculator(implicit amountsCalculator: BasicCalculator,
                                      previousPeriods:Seq[TaxYearResults], 
                                      contribution: Contribution) extends PeriodCalculator {
  val MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 60000 * 100L
  val P2AAA = 30000 * 100L
  val AA = 80000 * 100L
  val MAX_CF = 4000000L

  override def aaCCF(): Long = {
    if (!isTriggered) {
      actualUnused.slice(0, 4).map(_._2).foldLeft(0L)(_ + _)
    } else {
      if (isMPAAApplicable) {
        val aaa = (AAA + previous3YearsUnusedAllowance - preTriggerSavings)
        if (preTriggerSavings > AAA) (aaa).max(0) else (aaa).min(P2AAA)
      } else if (definedBenefit >= AA) {
        (AA + previous3YearsUnusedAllowance - postFlexiSavings).max(0)
      } else {
        (unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
      }
    }
  }

  override def aaCF(): Long = if (!isTriggered) annualAllowance + previous.availableAAWithCCF else previous.availableAAWithCF

  override def acaCF() : Long = if (isTriggered) 0L else (AAA + previous3YearsUnusedAllowance) - preFlexiSavings

  override def alternativeAA(): Long = if (isMPAAApplicable && isTriggered) AAA else 0L

  override def alternativeChargableAmount(): Long = if (isMPAAApplicable && isTriggered) mpist + dbist else 0L

  override def annualAllowance(): Long = if (!isTriggered) AA else if (defaultChargableAmount >= alternativeChargableAmount) AA else AAA

  def basicCalculator(): BasicCalculator = amountsCalculator

  override def chargableAmount(): Long = if (!isTriggered) basicCalculator().chargableAmount else if (isMPAAApplicable) alternativeChargableAmount.max(defaultChargableAmount) else defaultChargableAmount

  override def cumulativeDB(): Long = definedBenefit

  override def cumulativeMP(): Long = definedContribution
  
  override def dbist(): Long = {
    def isBefore2015(taxYearResult: TaxYearResults): Boolean = !(taxYearResult.input.isPeriod1 || taxYearResult.input.isPeriod2) && taxYearResult.input.taxPeriodStart.year <= 2015
    val year2014CCF = previousPeriods.filter(isBefore2015).headOption.map(_.summaryResult).getOrElse(SummaryResult()).availableAAWithCCF

    if (isTriggered) {
      val unusedaaa = preTriggerFields.map(_.unusedAAA).getOrElse(0L)
      val allowances = unusedaaa + year2014CCF
      if (definedBenefit < allowances) {
        0L
      } else {
        (allowances - definedBenefit).max(0)
      }
    } else {
      (preTriggerSavings - year2014CCF).max(0)
    }
  }

  override def dcaCF() : Long = if (!isTriggered) 0L else (AA + previous3YearsUnusedAllowance) - postFlexiSavings

  // treat both money purchase and defined benefit as same prior to flexi access
  override def definedBenefit(): Long = if (isTriggered) contribution.definedBenefit + preTriggerSavings else contribution.definedBenefit + contribution.moneyPurchase

  override def defaultChargableAmount(): Long = {
    if (!isTriggered) {
      0L
    } else {
      val aa = AA + previous3YearsUnusedAllowance
      if (postFlexiSavings > aa) {
        postFlexiSavings - aa
      } else {
        0L
      }
    }
  }

  override def exceedingAAA(): Long = if (isMPAAApplicable) (definedBenefit - AAA).max(0) else 0L

  override def exceedingAllowance(): Long = if (isTriggered) ((definedBenefit + definedContribution) - AA).max(0) else basicCalculator().exceedingAllowance

  override def exceedingMPAA(): Long = if (isMPAAApplicable) definedContribution - MPA else 0L

  override def isMPAAApplicable(): Boolean = definedContribution > MPA

  override def moneyPurchaseAA(): Long = if (isMPAAApplicable && isTriggered) (MPA - definedContribution).max(0) else 0L

  override def mpist(): Long = if (isMPAAApplicable) definedContribution - MPA else definedContribution

  override def preFlexiSavings() : Long = if (isTriggered) preTriggerSavings() else definedContribution + definedBenefit

  def preTriggerSavings(): Long = preTriggerInputs.map((c)=>c.definedBenefit+c.moneyPurchase).getOrElse(0L)

  def previous(): Summary = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())

  override def postFlexiSavings() : Long = if (isTriggered) definedContribution + definedBenefit else 0L

  override def unusedAAA(): Long = if (isMPAAApplicable && isTriggered) (AAA - definedBenefit).min(P2AAA).max(0) else 0L

  override def unusedAllowance(): Long = {
    if (!isTriggered) {
      basicCalculator().unusedAllowance.min(MAX_CF)
    } else {
      if (isMPAAApplicable) {
        0L
      } else {
        val unusedAllowance = {
          val savings = if (defaultChargableAmount >= alternativeChargableAmount) {
            preTriggerSavings + definedContribution
          } else {
            preTriggerSavings
          }
          if (savings > AA) 0L else (AA - savings).min(MAX_CF)
        }
        unusedAllowance.max(0)
      }
    }
  }

  override def unusedMPAA(): Long = if (isTriggered && !isMPAAApplicable) if ((MPA - definedContribution) > P2MPA) P2MPA else MPA - definedContribution else 0L
}
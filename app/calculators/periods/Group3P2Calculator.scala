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

case class Group3P2Calculator(implicit amountsCalculator: BasicCalculator,
                                       previousPeriods:Seq[TaxYearResults], 
                                       contribution: Contribution) extends PeriodCalculator {
  val group2P2Calculator = Group1P2Calculator()
  val MPA = 10000 * 100L
  val AAA = 30000 * 100L

  override def isMPAAApplicable(): Boolean = definedContribution > MPA

  override def definedBenefit(): Long = contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)

  override def dbist(): Long = {
    if (isPeriod1Triggered) {
      val allowances = (preTriggerFields.get.unusedAAA + period1.availableAAWithCCF)
      if (definedBenefit < allowances) {
        0L
      } else {
        (definedBenefit - allowances).max(0)
      }
    } else {
      val db = preTriggerAmounts.map {
        (amounts) =>
        amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)
      }.getOrElse(0L)
      (db - period1.availableAAWithCCF).max(0)
    }
  }

  override def mpist(): Long = {
    if (isPeriod1Triggered) {
      (definedContribution - period1Triggered.get.unusedMPAA).max(0)
    } else if (contribution.isTriggered) {
      if (isMPAAApplicable) {
        (definedContribution - MPA).max(0)
      } else {
        0L
      }
    } else {
      0L
    }
  }

  override def moneyPurchaseAA(): Long = period1.unusedMPAA

  override def alternativeAA(): Long = period1.unusedAAA

  override def alternativeChargableAmount(): Long = {
    if (isMPAAApplicable || (isPeriod1Triggered && period1Triggered.get.isMPA)) {
      (mpist + dbist).max(0)
    } else {
      0L
    }
  }

  override def defaultChargableAmount(): Long = {
    if (contribution.isTriggered) {
      period1Triggered.map {
        (fields) =>
        if (fields.isMPA) {
          val previous = previousPeriods.headOption.map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
          /*
            // scenario 26 only 
            val savings = previous.preFlexiSavings + previous.postFlexiSavings
            val aacf = previous.availableAAWithCF
            postFlexiSavings - previous.dcaCF
          */
          ((definedContribution + definedBenefit) - (previous3YearsUnusedAllowance + fields.unusedAAA)).max(0)
        } else {
          ((definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      }.getOrElse {
        val preTriggerSavings = preTriggerAmounts.map {
          (amounts) =>
          amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)
        }.getOrElse(0L)
        if (isMPAAApplicable) {
          ((preTriggerSavings + definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
        } else {
          ((definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      }
    } else {
      0L
    }
  }

  override def exceedingAllowance(): Long = if (isTriggered) 0L else group2P2Calculator.exceedingAllowance

  override def annualAllowance(): Long = period1.unusedAllowance

  override def unusedAllowance(): Long = {
    if (isTriggered) {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      val preTriggerSavings = amounts.moneyPurchase.getOrElse(0L) + amounts.definedBenefit.getOrElse(0L)
  
      val allowances = period1.unusedAllowance + previous3YearsUnusedAllowance
      val unusedAllowance = if (allowances < preTriggerSavings) {
        period1.unusedAAA - definedBenefit
      } else {
        val something = (definedContribution + definedBenefit) + amounts.moneyPurchase.getOrElse(0L)
        if (something > 4000000L) {
          0L
        } else {
          period1.unusedAllowance - definedBenefit
        }
      }
      unusedAllowance.max(0)
    } else {
      group2P2Calculator.unusedAllowance
    }
  }

  override def chargableAmount(): Long = {
    if (isTriggered) {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    } else {
      group2P2Calculator.chargableAmount
    }
  }

  override def aaCF(): Long = if (isTriggered) period1.availableAAWithCCF else group2P2Calculator.aaCCF

  override def aaCCF(): Long = {
    if (isTriggered) {
      val preTriggerSavings = previousPeriods.headOption.map {
        (result) =>
        result.input.amounts.map((amounts)=>amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)).getOrElse(0L)
      }.getOrElse(0L)
      val unused = unusedAllowance
      if (unused == 0L) {
        0L
      } else if (unused > 0) {
        if (definedBenefit == 0) {
          unusedAllowance
        } else {
          (unusedAllowance + previous2YearsUnusedAllowance)
        }
      } else {
        (previous3YearsUnusedAllowance - preTriggerSavings).max(0)
      }
    } else {
      group2P2Calculator.aaCCF
    }
  }

  override def cumulativeMP(): Long = {
    definedContribution + period1.cumulativeMP
  }

  override def cumulativeDB(): Long = {
    definedBenefit + period1.cumulativeDB
  }

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      definedContribution - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = 0L

  override def unusedAAA(): Long = if (isTriggered) period1.unusedAAA else 0L

  override def unusedMPAA(): Long = 0L

  override def preFlexiSavings() : Long = {
    if (isTriggered) {
      preTriggerAmounts.get.definedBenefit.getOrElse(0L) + preTriggerAmounts.get.moneyPurchase.getOrElse(0L)
    } else {
      definedContribution + definedBenefit
    }
  }

  override def postFlexiSavings() : Long = {
    if (isTriggered) {
      definedContribution + definedBenefit
    } else {
      0L
    }
  }
}

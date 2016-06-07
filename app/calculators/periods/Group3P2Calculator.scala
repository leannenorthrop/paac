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

  def basicCalculator(): BasicCalculator = amountsCalculator

  def preTriggerSavings(): Long = {
    val amounts = preTriggerAmounts.getOrElse(InputAmounts())
    amounts.moneyPurchase.getOrElse(0L) + amounts.definedBenefit.getOrElse(0L)
  }

  override def isMPAAApplicable(): Boolean = flexiAccessSavings > MPA

  override def definedBenefit(): Long = if (!isPeriod1Triggered && isTriggered) {
      previousInputs.definedBenefit.getOrElse(0L)
    } else {
      contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)
    }

  override def definedContribution(implicit contribution:Contribution): Long = if (!isPeriod1Triggered && isTriggered) {
      previousInputs.moneyPurchase.getOrElse(0L)
    } else {
      contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
    }
    
  def flexiAccessSavings(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  override def dbist(): Long = {
    if (isPeriod1Triggered) {
      val allowances = (preTriggerFields.get.unusedAAA + period1.availableAAWithCCF)
      if (definedBenefit < allowances) {
        0L
      } else {
        (definedBenefit - allowances).max(0)
      }
    } else {
      (preTriggerSavings - period1.availableAAWithCCF).max(0)
    }
  }

  override def mpist(): Long = {
    if (isPeriod1Triggered) {
      (flexiAccessSavings - period1Triggered.get.unusedMPAA).max(0)
    } else if (contribution.isTriggered) {
      if (isMPAAApplicable) {
        (flexiAccessSavings - MPA).max(0)
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
          ((flexiAccessSavings + definedBenefit) - (previous3YearsUnusedAllowance + fields.unusedAAA)).max(0)
        } else {
          ((flexiAccessSavings + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      }.getOrElse {
        if (isMPAAApplicable) {
          //((preTriggerSavings + flexiAccessSavings + definedBenefit) - period1.availableAAWithCCF).max(0)
          ((preTriggerSavings + flexiAccessSavings) - period1.availableAAWithCCF).max(0)
        } else {
          //((flexiAccessSavings + definedBenefit) - period1.availableAAWithCCF).max(0)
          ((flexiAccessSavings) - period1.availableAAWithCCF).max(0)
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
      val allowances = period1.unusedAllowance + previous3YearsUnusedAllowance

      val unusedAllowance = if (allowances < preTriggerSavings) {
        period1.unusedAAA - definedBenefit
      } else {
        val amounts = preTriggerAmounts.getOrElse(InputAmounts())
        val something = (flexiAccessSavings + definedBenefit) + amounts.moneyPurchase.getOrElse(0L)

        if (something > 4000000L) {
          //period1.unusedAllowance
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
      val unused = unusedAllowance
      if (unused > 0) {
        (unused + previous2YearsUnusedAllowance)
      } else {
        0L
      }
    } else {
      group2P2Calculator.aaCCF
    }
  }

  override def cumulativeMP(): Long = {
    flexiAccessSavings + period1.cumulativeMP
  }

  override def cumulativeDB(): Long = {
    definedBenefit + period1.cumulativeDB
  }

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      flexiAccessSavings - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = 0L

  override def unusedAAA(): Long = if (isTriggered) period1.unusedAAA else 0L

  override def unusedMPAA(): Long = 0L

  override def preFlexiSavings() : Long = {
    if (isTriggered) {
      preTriggerSavings()
    } else {
      definedContribution + definedBenefit
    }
  }

  override def postFlexiSavings() : Long = {
    if (isTriggered) {
      flexiAccessSavings + definedBenefit
    } else {
      0L
    }
  }
}

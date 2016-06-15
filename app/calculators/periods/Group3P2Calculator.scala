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
  val P1MPA = 20000 * 100L
  val MAXAACF = 40000 * 100L

  def basicCalculator(): BasicCalculator = amountsCalculator

  def sum(values: List[Option[Long]]): Long = values.map(_.getOrElse(0L)).foldLeft(0L)(_+_)

  def preTriggerSavings(): Long = {
    val values = if (!isPeriod1Triggered && isTriggered) {
      val p2BeforeTrigger = preTriggerAmounts.getOrElse(InputAmounts())
      List(period1Amounts,p2BeforeTrigger).flatMap((v)=>List(v.moneyPurchase, v.definedBenefit))
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      List(amounts.moneyPurchase,amounts.definedBenefit)
    }
    sum(values)
  }

  def postTriggerSavings(): Long = {
    if (!isPeriod1Triggered && isTriggered) {
      definedBenefit + definedContribution
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      definedBenefit + definedContribution + period1Amounts.definedBenefit.getOrElse(0L) + period1Amounts.moneyPurchase.getOrElse(0L)
    }
  }

  def period2PreTriggerSavings(): Long = {
    if (isPeriod1Triggered) {
      0L
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      sum(List(amounts.moneyPurchase,amounts.definedBenefit))
    }
  }

  override def isMPAAApplicable(): Boolean = (flexiAccessSavings > MPA) || period1.isMPA || period1Amounts.moneyPurchase.getOrElse(0L) >= P1MPA

  override def definedBenefit(): Long = if (!isPeriod1Triggered && isTriggered) {
      previousInputs.definedBenefit.getOrElse(0L)
    } else {
      contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)
    }

  def p2definedBenefit(): Long = contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)

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
      (period2PreTriggerSavings - period1.availableAAWithCCF).max(0)
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
          ((period2PreTriggerSavings + flexiAccessSavings) - period1.availableAAWithCCF).max(0)
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
      val unusedAllowance = if (period1.isMPA) {
        period1.unusedAAA - definedBenefit
      } else {
        val savings = if (defaultChargableAmount >= alternativeChargableAmount) {
          period2PreTriggerSavings + postTriggerSavings
        } else {
          period2PreTriggerSavings
        }
        val deduct = if (isPeriod1Triggered) p2definedBenefit else period2PreTriggerSavings
        if (savings > MAXAACF) 0L else period1.unusedAllowance - deduct
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
        unused + previous2YearsUnusedAllowance
      } else {
        val ccf = (previous2YearsUnusedAllowance - period1NotTriggered.map(_.exceedingAAAmount).getOrElse(0L)).max(0L)
        if (ccf > previous.availableAAWithCCF) 0L else ccf
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
      period2PreTriggerSavings()
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

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

case class Group2P1Calculator(implicit amountsCalculator: BasicCalculator,
                                       previousPeriods:Seq[TaxYearResults], 
                                       contribution: Contribution) extends PeriodCalculator {
  val MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 60000 * 100L
  val P2AAA = 30000 * 100L
  val AA = 80000 * 100L
  val MAX_CF = 4000000L
  val group1Calculator = Group1P1Calculator()

  def basicCalculator(): BasicCalculator = amountsCalculator
  
  override def isMPAAApplicable(): Boolean = definedContribution > MPA

  def sum(values: List[Option[Long]]): Long = values.map(_.getOrElse(0L)).foldLeft(0L)(_+_)

  def preTriggerSavings(): Long = {
    val amounts = preTriggerAmounts.getOrElse(InputAmounts())
    sum(List(amounts.moneyPurchase,amounts.definedBenefit))
  }

  def postTriggerSavings(): Long = {
    if (isTriggered) {
      definedContribution
    } else {
      0L
    }
  }

  override def definedBenefit(): Long = {
    contribution.amounts.map {
      (amounts) =>
      if (!isTriggered)
        amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
      else {
        // treat both money purchase and defined benefit as same prior to flexi access
        amounts.definedBenefit.getOrElse(0L) + (previousInputs.definedBenefit.getOrElse(0L) + previousInputs.moneyPurchase.getOrElse(0L)) 
      }
    }.getOrElse(0L)
  }

  def year2014CCF(): Long = pre2015Results.headOption.map(_.summaryResult).getOrElse(SummaryResult()).availableAAWithCCF

  override def dbist(): Long = {
    if (isTriggered) {
      val allowances = (preTriggerFields.getOrElse(ExtendedSummaryFields()).unusedAAA + year2014CCF)
      if (definedBenefit < allowances) {
        0L
      } else {
        (allowances - definedBenefit).max(0)
      }
    } else {
      val db = preTriggerAmounts.map {
        (amounts) =>
        amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)
      }.getOrElse(0L)
      (db - year2014CCF).max(0)
    }
  }

  override def mpist(): Long = {
    if (isMPAAApplicable) {
      definedContribution - MPA
    } else {
      definedContribution
    }
  }

  override def moneyPurchaseAA(): Long = {
    if (!isTriggered) {
      0L
    } else {
      if (isMPAAApplicable) {
        (MPA - definedContribution).max(0)
      } else {
        0L
      }
    }
  }

  override def alternativeAA(): Long = {
    if (!isTriggered) {
      0L
    } else {
      if (isMPAAApplicable) {
        AAA
      } else {
        0L
      }
    }
  }

  override def alternativeChargableAmount(): Long = {
    if (!isTriggered) {
      0L
    } else {
      if (isMPAAApplicable) {
        mpist + dbist
      } else {
        0L
      }
    }
  }

  override def defaultChargableAmount(): Long = {
    if (!isTriggered) {
      0L
    } else {
      val savings = definedBenefit + definedContribution
      val aa = AA + previous3YearsUnusedAllowance
      if (savings > aa) {
        savings - aa
      } else {
        0L
      }
    }
  }

  override def exceedingAllowance(): Long = {
    if (!isTriggered) {
      group1Calculator.exceedingAllowance
    } else {
      if (isMPAAApplicable) {
        if (definedBenefit + definedContribution > AA) {
          (definedBenefit + definedContribution) - AA
        } else {
          0L
        }
      } else {
        ((definedBenefit + definedContribution) - AA).max(0)
      }
    }
  }

  override def annualAllowance(): Long = {
    if (!isTriggered) {
      group1Calculator.annualAllowance
    } else if (defaultChargableAmount >= alternativeChargableAmount) {
      AA
    } else {
      AAA
    }
  }

  override def unusedAllowance(): Long = {
    if (!isTriggered) {
      group1Calculator.unusedAllowance
    } else {
      if (isMPAAApplicable) {
        0L
      } else {
        val unusedAllowance = {
          val savings = if (defaultChargableAmount >= alternativeChargableAmount) {
            preTriggerSavings + postTriggerSavings
          } else {
            preTriggerSavings
          }
          if (savings > AA) 0L else (AA - savings).min(MAX_CF)
        }
        unusedAllowance.max(0)
      }
    }
  }

  override def chargableAmount(): Long = {
    if (!isTriggered) {
      group1Calculator.chargableAmount
    } else {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    }
  }

  override def aaCF(): Long = {
    if (!isTriggered) {
      group1Calculator.aaCF
    } else {
      previousResults.map(_.summaryResult.availableAAWithCF).getOrElse(0L)
    }
  }

  override def aaCCF(): Long = {
    if (!isTriggered) {
      group1Calculator.aaCCF
    } else {
      val annualAllowance = AA
      if (isMPAAApplicable) {
        if (preTriggerSavings > AAA) {
          ((AAA + previous3YearsUnusedAllowance - preTriggerSavings)).max(0)
        } else {
          ((AAA + previous3YearsUnusedAllowance - preTriggerSavings)).min(P2AAA)
        }
      } else if (definedBenefit >= annualAllowance) {
        (annualAllowance + previous3YearsUnusedAllowance - (definedBenefit+definedContribution)).max(0)
      } else {
        (unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
      }
    }
  }

  override def cumulativeMP(): Long = definedContribution

  override def cumulativeDB(): Long = definedBenefit

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      definedContribution - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = {
    if (isMPAAApplicable) {
      (definedBenefit - AAA).max(0)
    } else {
      0L
    }
  }

  override def unusedAAA(): Long = {
    if (!isTriggered) {
      0L
    } else if (alternativeChargableAmount > defaultChargableAmount) {
      (AAA - definedBenefit).min(P2AAA).max(0)
    } else {
      0L
    }
  }

  override def unusedMPAA(): Long = {
    if (!isTriggered) {
      0L
    } else if (definedContribution < MPA) {
      val v = MPA - definedContribution
      if (v > P2MPA) {
        P2MPA
      } else {
        v
      }
    } else {
      0L
    }
  }

  override def preFlexiSavings() : Long = {
    if (isTriggered) {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts(Some(0), Some(0), None, Some(false)))
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
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

  override def acaCF() : Long = {
    if (isTriggered) {
      0L
    } else {
     (AAA + previous3YearsUnusedAllowance) - preFlexiSavings
    }
  }

  override def dcaCF() : Long = {
    if (!isTriggered) {
      0L
    } else {
      (AA + previous3YearsUnusedAllowance) - postFlexiSavings
    }
  }
}
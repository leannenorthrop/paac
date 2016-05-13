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
  me: Group2P1Calculator => 

  val MPA = 20000 * 100L
  val AAA = 60000 * 100L
  val AA = 80000 * 100L
  val MAX_CF = 4000000L
  val group1Calculator = Group1P1Calculator()

  def preTriggerFields(): Option[ExtendedSummaryFields] = {
    previousPeriods.find(!_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields])
  }

  def preTriggerAmounts(): Option[InputAmounts] = {
    previousPeriods.find(!_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).flatMap(_.input.amounts)
  }

  override def isMPAAApplicable(): Boolean = {
    if (!contribution.isTriggered) {
      super.isMPAAApplicable()
    } else {
      me.definedContribution > MPA && contribution.isTriggered
    }
  }

  override def definedBenefit(): Long = {
    contribution.amounts.map {
      (amounts) =>
      if (!contribution.isTriggered)
        amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
      else {
        val preFlexiAccessSavings = previousPeriods.headOption.map(_.input.amounts.get).getOrElse(InputAmounts())
        // treat both money purchase and defined benefit as same prior to flexi access
        val db = (preFlexiAccessSavings.definedBenefit.getOrElse(0L) + preFlexiAccessSavings.moneyPurchase.getOrElse(0L)) 
        amounts.definedBenefit.getOrElse(0L) + db
      }
    }.getOrElse(0L)
  }

  override def dbist(): Long = {
    val previousYear = previousPeriods.find(_.input.taxPeriodStart.year != 2015).headOption.map(_.summaryResult).getOrElse(SummaryResult())
    if (contribution.isTriggered) {
      val allowances = (me.preTriggerFields.get.unusedAAA + previousYear.availableAAWithCCF)
      if (me.definedBenefit < allowances) {
        0L
      } else {
        (allowances - me.definedBenefit).max(0)
      }
    } else {
      val db = me.preTriggerAmounts.map {
        (amounts) =>
        amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)
      }.getOrElse(0L)
      (db - previousYear.availableAAWithCCF).max(0)
    }
  }

  override def mpist(): Long = {
    if (me.isMPAAApplicable){
      me.definedContribution - MPA
    } else {
      me.definedContribution
    }
  }

  override def moneyPurchaseAA(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
      if (me.isMPAAApplicable) {
        val v = MPA - me.definedContribution
        if (v > 1000000L) {
          1000000L
        } else {
          v
        }
      } else {
        0L
      }
    }
  }

  override def alternativeAA(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
      if (me.isMPAAApplicable) {
        AAA
      } else {
        0L
      }
    }
  }

  override def alternativeChargableAmount(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
      if (me.isMPAAApplicable) {
        val mpist = me.mpist
        val dbist = me.dbist
        mpist + dbist
      } else {
        0L
      }
    }
  }

  override def defaultChargableAmount(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
      val savings = me.definedBenefit + me.definedContribution
      val aa = AA + previous3YearsUnusedAllowance
      if (savings > aa) {
        savings - aa
      } else {
        0L
      }
    }
  }

  override def exceedingAllowance(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.exceedingAllowance
    } else {
      if (me.isMPAAApplicable) {
        if (me.definedBenefit + me.definedContribution > AA) {
          (me.definedBenefit + me.definedContribution) - AA
        } else {
          (AA - (me.definedBenefit + me.definedContribution)).min(MAX_CF)
        }
      } else {
        ((me.definedBenefit + me.definedContribution) - AA).max(0)
      }
    }
  }

  override def annualAllowance(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.annualAllowance
    } else if (me.defaultChargableAmount >= me.alternativeChargableAmount) {
      AA
    } else {
      AAA
    }
  }

  override def unusedAllowance(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.unusedAllowance
    } else if (me.isMPAAApplicable) {
      0L
    } else {
      if (me.definedBenefit + me.definedContribution > AA) {
        0L
      } else {
        (AA - (me.definedBenefit + me.definedContribution)).min(MAX_CF)
      }
    }
  }

  override def chargableAmount(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.chargableAmount
    } else {
      val dca = me.defaultChargableAmount
      if (me.isMPAAApplicable) {
        val aca = me.alternativeChargableAmount
        aca.max(dca) // if aca == dca then choose dca
      } else {
        dca
      }
    }
  }

  override def aaCF(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.aaCF
    } else {
      AA + me.previous3YearsUnusedAllowance
    }
  }

  override def aaCCF(): Long = {
    if (!contribution.isTriggered) {
      group1Calculator.aaCCF
    } else {
      val definedBenefit = me.definedBenefit
      val annualAllowance = AA
      val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance
      if (isMPAAApplicable) {
        val amounts = me.preTriggerAmounts.getOrElse(InputAmounts())
        val preSavings = amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
        ((AAA + previous3YearsUnusedAllowance - preSavings).min(3000000L)).max(0)
      } else if (definedBenefit >= annualAllowance) {
        (annualAllowance + previous3YearsUnusedAllowance - (definedBenefit+definedContribution)).max(0)
      } else {
        (me.unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
      }
    }
  }

  override def cumulativeMP(): Long = {
    me.definedContribution
  }

  override def cumulativeDB(): Long = {
    me.definedBenefit
  }

  override def exceedingMPAA(): Long = {
    if (me.isMPAAApplicable) {
      me.definedContribution - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = {
    if (me.isMPAAApplicable) {
      (me.definedBenefit - AAA).max(0)
    } else {
      0L
    }
  }

  override def unusedAAA(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else if (me.alternativeChargableAmount > me.defaultChargableAmount) {
      if (me.definedBenefit > AAA) {
        0L
      } else {
        val v = AAA - me.definedBenefit
        if (v > 3000000L) {
          3000000L
        } else {
          AAA - me.definedBenefit
        }
      }
    } else {
      0L
    }
  }

  override def unusedMPAA(): Long = {
    if (!contribution.isTriggered) {
      0L
    } else if (me.definedContribution < MPA) {
      val v = MPA - me.definedContribution
      if (v > 1000000L) {
        1000000L
      } else {
        v
      }
    } else {
      0L
    }
  }

  override def preFlexiSavings() : Long = {
    if (contribution.isTriggered) {
      preTriggerAmounts.get.definedBenefit.getOrElse(0L) + preTriggerAmounts.get.moneyPurchase.getOrElse(0L)
    } else {
      me.definedContribution + me.definedBenefit
    }
  }

  override def postFlexiSavings() : Long = {
    if (contribution.isTriggered) {
      me.definedContribution + me.definedBenefit
    } else {
      0L
    }
  }

  override def acaCF() : Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
     (AAA + me.previous3YearsUnusedAllowance) - me.preFlexiSavings
    }
  }

  override def dcaCF() : Long = {
    if (!contribution.isTriggered) {
      0L
    } else {
      (AA + me.previous3YearsUnusedAllowance) - me.postFlexiSavings
    }
  }
}
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

case class Group2P1Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me: Group2P1Calculator => 

  val MPA = 20000*100L
  val AAA = 60000 * 100L
  val AA = 80000*100L
  val MAX_CF = 4000000L

  def isMPAAApplicable(implicit contribution: Contribution): Boolean = {
    me.definedContribution > MPA && contribution.isTriggered
  }

  def definedBenefit(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
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

  def dbist(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.definedBenefit > AAA) {
      me.definedBenefit - AAA
    } else {
      0L
    }
  }

  def mpist(implicit contribution:Contribution): Long = {
    if (me.definedContribution > MPA){
      me.definedContribution - MPA
    } else {
      0L
    }
  }

  def moneyPurchaseAA(implicit contribution:Contribution): Long = {
    if (me.definedContribution < MPA && me.isMPAAApplicable(contribution)) {
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

  def alternativeAA(implicit contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      AAA
    } else {
      0L
    }
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      val mpist = me.mpist
      val dbist = me.dbist
      mpist + dbist
    } else {
      0L
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val savings = me.definedBenefit + me.definedContribution
    val aa = AA + previous3YearsUnusedAllowance
    if (savings > aa) {
      savings - aa
    } else {
      0L
    }
  }

  def exceedingAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      if (me.definedBenefit + me.definedContribution > AA) {
        (me.definedBenefit + me.definedContribution) - AA
      } else {
        (AA - (me.definedBenefit + me.definedContribution)).min(MAX_CF)
      }
    } else {
      ((me.definedBenefit + me.definedContribution) - AA).max(0)
    }
  }

  def annualAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.defaultChargableAmount >= me.alternativeChargableAmount) {
      AA
    } else {
      AAA
    }
  }

  def unusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      0L
    } else {
      if (me.definedBenefit + me.definedContribution > AA) {
        0L
      } else {
        (AA - (me.definedBenefit + me.definedContribution)).min(MAX_CF)
      }
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val dca = me.defaultChargableAmount
    if (me.isMPAAApplicable(contribution)) {
      val aca = me.alternativeChargableAmount
      aca.max(dca) // if aca == dca then choose dca
    } else {
      dca
    }
  }

  def aaCF(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    AA + me.previous3YearsUnusedAllowance
  }

  def aaCCF(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val definedBenefit = me.definedBenefit
    val annualAllowance = AA
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (me.unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
    }
  }

  def cumulativeMP(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    me.definedContribution
  }

  def cumulativeDB(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    me.definedBenefit
  }

  def exceedingMPAA(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      me.definedContribution - MPA
    } else {
      0L
    }
  }

  def exceedingAAA(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      (me.definedBenefit - AAA).max(0)
    } else {
      0L
    }
  }

  def unusedAAA(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (me.alternativeChargableAmount > me.defaultChargableAmount) {
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

  def unusedMPAA(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (me.definedContribution < MPA) {
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

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    if (!contribution.isTriggered) {
      Group1P1Calculator(amountsCalculator).summary.map {
        (s)=>
        Group2Fields(chargableAmount=s.chargableAmount,
                     exceedingAAAmount=s.exceedingAAAmount,
                     availableAllowance=s.availableAllowance,
                     unusedAllowance=s.unusedAllowance,
                     availableAAWithCF=s.availableAAWithCF,
                     availableAAWithCCF=s.availableAAWithCCF,
                     dbist = me.dbist)
      }
    } else {
      Some(Group2Fields(me.chargableAmount,
                        me.exceedingAllowance,
                        me.annualAllowance,
                        me.unusedAllowance,
                        me.aaCF,
                        me.aaCCF,
                        0L,
                        me.moneyPurchaseAA,
                        me.alternativeAA,
                        me.dbist,
                        me.mpist,
                        me.alternativeChargableAmount,
                        me.defaultChargableAmount,
                        me.cumulativeMP,
                        me.cumulativeDB,
                        me.exceedingMPAA,
                        me.exceedingAAA,
                        me.unusedAAA,
                        me.unusedMPAA))
    }
  }
}
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

  def definedBenefit(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    contribution.amounts.map {
      (amounts) =>
      if (!contribution.isTriggered)
        amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
      else {
        val dbist = previousPeriods.headOption.map(_.summaryResult.asInstanceOf[Group2Fields].dbist).getOrElse(0L)
        amounts.definedBenefit.getOrElse(0L) + dbist
      }
    }.getOrElse(0L)
  }

  def dbist(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (!contribution.isTriggered)
      me.definedBenefit + me.definedContribution
    else {
      val preFlexiSavings = me.preFlexiSavings
      val aa = AAA + me.previous3YearsUnusedAllowance
      (aa - preFlexiSavings)
    }
  }

  def mpist(implicit contribution:Contribution): Long = {
    me.definedContribution-MPA
  }

  def moneyPurchaseAA(implicit contribution:Contribution): Long = {
    (MPA - me.definedContribution).max(0)
  }

  def alternativeAA(): Long = {
    AAA.min(30000*100L)
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (MPA > me.definedContribution) {
      0L
    } else {
      if (AAA > (me.definedBenefit+me.preFlexiSavings)) {
        if (me.definedContribution > AAA) {
          me.definedContribution - MPA
        } else {
          0L
        }
      } else {
        me.mpist + me.dbist.abs
      }
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val aa = AA + me.previous3YearsUnusedAllowance
    val preFlexiSavings = me.preFlexiSavings
    val postFlexiSavings = me.definedContribution
    val savings = preFlexiSavings + postFlexiSavings
    if (MPA > me.definedContribution) {
      0L
    } else {
      savings - AA
    }
  }

  def exceedingAllowance(implicit contribution:Contribution): Long = {
    (amountsCalculator.definedBenefit - me.annualAllowance).max(0)
  }

  def annualAllowance(): Long = AA

  def unusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (MPA > me.definedContribution) {
      val aa = AA + me.previous3YearsUnusedAllowance
      val preFlexiSavings = me.preFlexiSavings
      val postFlexiSavings = me.definedContribution
      val savings = preFlexiSavings + postFlexiSavings
      val unusedAllowance = (AA - savings).max(0)
      unusedAllowance.min(MAX_CF)
    } else {
      0L
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val charge = me.alternativeChargableAmount.max(me.defaultChargableAmount)
    if (charge <= 0) {
      (amountsCalculator.definedBenefit - me.annualAllowance).max(0)
    }
    else
      charge
  }

  def aaCF(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    AA + me.previous3YearsUnusedAllowance
  }

  def aaCCF(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val annualAllowance = amountsCalculator.annualAllowance
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (me.unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
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
                        cumulativeMP = 0,
                        cumulativeDB = 0,
                        exceedingMPAA = 0,
                        exceedingAAA = 0,
                        unusedAA = 0,
                        unusedMPAA = 0,
                        preFlexiSavings = me.preFlexiSavings,
                        postFlexiSavings = amountsCalculator.definedContribution))
    }
  }
}
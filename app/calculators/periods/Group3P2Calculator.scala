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

case class Group3P2Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me: Group3P2Calculator =>
  

  val MPA = 10000 * 100L
  val AAA = 30000 * 100L

  def period1(implicit previousPeriods:Seq[TaxYearResults]) = {
    previousPeriods.find(_.input.isPeriod1).headOption.map(_.summaryResult.asInstanceOf[Group2Fields]).getOrElse(Group2Fields())
  }

  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[Group2Fields] = {
    previousPeriods.find(!_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).map(_.summaryResult.asInstanceOf[Group2Fields])
  }

  def preTriggerAmounts(implicit previousPeriods:Seq[TaxYearResults]): Option[InputAmounts] = {
    previousPeriods.find(!_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).flatMap(_.input.amounts)
  }

  def period1Triggered(implicit previousPeriods:Seq[TaxYearResults]): Option[Group2Fields] = {
    previousPeriods.find(_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).map(_.summaryResult.asInstanceOf[Group2Fields])
  }

  def isMPAAApplicable(implicit contribution: Contribution): Boolean = {
    me.definedContribution > MPA
  }

  def definedBenefit(implicit contribution: Contribution): Long = contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)

  def dbist(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    val period1Triggered = me.preTriggerFields
    if (period1Triggered.isDefined) {
      val allowances = (preTriggerFields.get.unusedAAA + me.previous3YearsUnusedAllowance)
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
      (db - AAA)
    }
  }

  def mpist(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val period1Triggered = me.period1Triggered
    if (period1Triggered.isDefined) {
      me.definedContribution - period1Triggered.get.unusedMPAA
    } else {
      if (isMPAAApplicable) {
        me.definedContribution - MPA
      } else {
        0L
      }
    }
  }

  def moneyPurchaseAA(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    period1.unusedMPAA
  }

  def alternativeAA(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    period1.unusedAAA
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      (me.mpist + me.dbist).max(0)
    } else {
      0L
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    period1Triggered.map {
      (fields) =>
      if (fields.isMPA) {
        ((me.definedContribution + me.definedBenefit) - (me.previous3YearsUnusedAllowance + fields.unusedAAA)).max(0)
      } else {
        ((me.definedContribution + me.definedBenefit) - (me.previous3YearsUnusedAllowance + fields.unusedAllowance)).max(0)
      }
    }.getOrElse {
      val preTriggerSavings = me.preTriggerAmounts.map {
        (amounts) =>
        amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)
      }.getOrElse(0L)
      if (me.isMPAAApplicable(contribution)) {
        ((preTriggerSavings + me.definedContribution + me.definedBenefit) - (me.previous3YearsUnusedAllowance + period1.unusedAllowance)).max(0)
      } else {
        ((me.definedContribution + me.definedBenefit) - (me.previous3YearsUnusedAllowance + period1.unusedAllowance)).max(0)
      }
    }
  }

  def exceedingAllowance(): Long = 0L

  def annualAllowance(implicit previousPeriods:Seq[TaxYearResults]): Long = period1.unusedAllowance

  def unusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val preTriggerSavings = previousPeriods.headOption.map {
      (result) =>
      result.input.amounts.map((amounts)=>amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)).getOrElse(0L)
    }.getOrElse(0L)

    val allowances = period1.unusedAllowance + me.previous3YearsUnusedAllowance
    if (allowances < preTriggerSavings) {
      period1.unusedAAA - me.definedBenefit
    } else {
      preTriggerSavings - allowances
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

  def aaCF(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    period1.availableAAWithCCF
  }

  def aaCCF(implicit previousPeriods: Seq[TaxYearResults], contribution:Contribution): Long = {
    val preTriggerSavings = previousPeriods.headOption.map {
      (result) =>
      result.input.amounts.map((amounts)=>amounts.definedBenefit.getOrElse(0L)+amounts.moneyPurchase.getOrElse(0L)).getOrElse(0L)
    }.getOrElse(0L)
    val unused = me.unusedAllowance
    if (unused == 0L) {
      0L
    } else if (unused > 0) {
      (me.unusedAllowance + me.previous2YearsUnusedAllowance)
    } else {
      (me.previous3YearsUnusedAllowance - preTriggerSavings).max(0)
    }
  }

  def cumulativeMP(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    me.definedContribution + period1.cumulativeMP
  }

  def cumulativeDB(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    me.definedBenefit + period1.cumulativeDB
  }

  def exceedingMPAA(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      me.definedContribution - MPA
    } else {
      0L
    }
  }

  def exceedingAAA(): Long = 0L

  def unusedAAA(implicit previousPeriods:Seq[TaxYearResults]): Long = period1.unusedAAA

  def unusedMPAA(): Long = 0L

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    if (!contribution.isTriggered) {
      Group1P2Calculator(amountsCalculator).summary.map {
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
                        me.unusedMPAA,
                        0,
                        0,
                        me.isMPAAApplicable))
    }
  }
}

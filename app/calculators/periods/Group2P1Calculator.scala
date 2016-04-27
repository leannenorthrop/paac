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
/*
class G2P1Helper(previousPeriods:Seq[TaxYearResults], 
                 contribution: Contribution, 
                 amountsCalculator: BasicCalculator, 
                 additionalFields: Option[AdditionalFields] = None) {
  me => G2P1Helper
  val MPA = 20000*100L
  val AAA = 60000 * 100L
  val AA = 80000*100L
  val MAX_CF = 4000000L

  def previous3YearsUnusedAllowance(): Long = previousPeriods.filterNot(_.additionalFields == None).slice(0,3).foldLeft(0L)(_+_.summaryResult.unusedAllowance)

  def preFlexiSavings(): Long = {
    previousPeriods.filterNot(_.additionalFields == None).foldLeft(0L)(_+_.additionalFields.get.dbist)
  }

  def definedBenefit(): Long = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    if (!isTriggered)
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
    else {
      val dbist = previousPeriods.headOption.map(_.additionalFields.map(_.dbist).getOrElse(0L)).getOrElse(0L)
      amounts.definedBenefit.getOrElse(0L) + dbist
    }
  }

  def definedContribution(): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def dbist(): Long = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    if (!isTriggered)
      me.definedBenefit + me.definedContribution
    else {
      val preFlexiSavings = me.preFlexiSavings
      val aa = AAA + me.previous3YearsUnusedAllowance
      (aa - preFlexiSavings)
    }
  }

  def mpist(): Long = {
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    (amounts.moneyPurchase.getOrElse(0L)-MPA)
  }

  def moneyPurchaseAA(): Long = {
    val mpaa = (MPA - me.definedContribution).max(0)
    mpaa
  }

  def alternativeAA(): Long = {
    AAA.min(30000*100L)
  }

  def alternativeChargableAmount(): Long = {
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

  def defaultChargableAmount(): Long = {
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
}

case class Group2P1Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me => Group2P1Calculator

  val MPA = 20000*100L
  val AAA = 60000 * 100L
  val AA = 80000*100L
  val MAX_CF = 4000000L

  def exceedingAllowance()(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    (amountsCalculator.definedBenefit - me.annualAllowance).max(0)
  }

  def annualAllowance()(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    AA
  }

  def unusedAllowance(additionalFields: AdditionalFields)(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    if (MPA > amountsCalculator.definedContribution) {
      val aa = AA + me.previous3YearsUnusedAllowance
      val preFlexiSavings = additionalFields.preFlexiSavings
      val postFlexiSavings = additionalFields.postFlexiSavings
      val savings = preFlexiSavings + postFlexiSavings
      val unusedAllowance = (AA - savings).max(0)
      unusedAllowance.min(MAX_CF)
    } else {
      0L
    }
  }

  def chargableAmount(additionalFields: AdditionalFields)(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    val charge = additionalFields.alternativeChargableAmount.max(additionalFields.defaultChargableAmount)
    if (charge <= 0) {
      (amountsCalculator.definedBenefit - me.annualAllowance).max(0)
    }
    else
      charge
  }

  def aaCF(additionalFields: AdditionalFields)(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    AA + me.previous3YearsUnusedAllowance
  }

  def aaCCF(additionalFields: AdditionalFields)(implicit previousPeriods:Seq[SummaryResult], contribution:Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val annualAllowance = amountsCalculator.annualAllowance
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (me.unusedAllowance(additionalFields).min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
    }
  }

  def additional(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution) : Option[AdditionalFields] = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    val helper = new G2P1Helper(previousPeriods, contribution, amountsCalculator)
    if (!isTriggered) {
      Some(AdditionalFields(dbist=helper.dbist))
    } else {
      Some(AdditionalFields(helper.moneyPurchaseAA,
                            helper.alternativeAA,
                            helper.dbist,
                            helper.mpist,
                            helper.alternativeChargableAmount,
                            helper.defaultChargableAmount,
                            cumulativeMP = 0,
                            cumulativeDB = 0,
                            exceedingMPAA = 0,
                            exceedingAAA = 0,
                            unusedAA = 0,
                            unusedMPAA = 0,
                            preFlexiSavings = helper.preFlexiSavings,
                            postFlexiSavings = amountsCalculator.definedContribution(previousPeriods.map(_.summaryResult),contribution)))
    }
  }

  def summary(additionalFields: Option[AdditionalFields] = None)(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    if (!isTriggered) {
      Group1P1Calculator(amountsCalculator).summary(additionalFields)
    } else {
      additionalFields.map {
        (fields) =>
        SummaryResult(me.chargableAmount(fields), 
                      me.exceedingAllowance(), 
                      me.annualAllowance(), 
                      me.unusedAllowance(fields), 
                      me.aaCF(fields), 
                      me.aaCCF(fields))
      }
    }
  }
}*/
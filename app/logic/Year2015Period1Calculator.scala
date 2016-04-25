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

package logic

import config.PaacConfiguration
import models._

case class Group1P1Calculator(amountsCalculator: BasicAmountsCalculator) {
  me => Group1P2Calculator

  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def aaCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = amountsCalculator.annualAllowance + previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def aaCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val annualAllowance = amountsCalculator.annualAllowance
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance()
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (amountsCalculator.unusedAllowance.min(4000000L) + previous3YearsUnusedAllowance).max(0)
    }
  }

  def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    Some(SummaryResult(amountsCalculator.chargableAmount, 
                       amountsCalculator.exceedingAllowance, 
                       amountsCalculator.annualAllowance, 
                       amountsCalculator.unusedAllowance.min(4000000L), 
                       me.aaCF, 
                       me.aaCCF, 
                       0L))
  }
}

case class Group2P1Calculator(amountsCalculator: BasicAmountsCalculator) {
  me => Group2P1Calculator
  val MPA = 20000*100L
  val AAA = 60000 * 100L
  val AA = 80000*100L
  val MAX_CF = 4000000L

  def previous3YearsUnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.filterNot(_.dbist > 0).slice(0,3).foldLeft(0L)(_+_.unusedAllowance)

  def preFlexiSavings(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    previousPeriods.filter(_.dbist > 0).foldLeft(0L)(_+_.dbist)
  }

  def definedBenefit(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    if (!isTriggered)
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
    else {
      val dbist = previousPeriods.headOption.map(_.dbist).getOrElse(0L)
      amounts.definedBenefit.getOrElse(0L) + dbist
    }
  }

  def definedContribution(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def dbist(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    if (!isTriggered)
      me.definedBenefit + me.definedContribution
    else {
      val preFlexiSavings = me.preFlexiSavings
      val aa = AAA + me.previous3YearsUnusedAllowance
      (aa - preFlexiSavings)
    }
  }

  def mpist(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    (amounts.moneyPurchase.getOrElse(0L)-MPA).max(0)
  }

  def moneyPurchaseAA(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val mpaa = (MPA - me.definedContribution).max(0)
    mpaa
  }

  def alternativeAA(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    AAA.min(30000*100L)
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    if (MPA > me.definedContribution) {
      -1L // N/A because less than MPAA
    } else {
      if (AAA > (me.definedBenefit+me.preFlexiSavings)) {
        -1L
      } else {
        me.mpist + me.dbist.abs
      }
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
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

  def exceedingAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    (me.definedBenefit - me.annualAllowance).max(0)
  }

  def annualAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    AA
  }

  def unusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
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

  def chargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val charge = me.alternativeChargableAmount.max(me.defaultChargableAmount)
    if (charge <= 0) {
      (me.definedBenefit - me.annualAllowance).max(0)
    }
    else
      charge
  }

  def aaCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val aa = AA + me.previous3YearsUnusedAllowance
    aa
  }

  def aaCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedBenefit = amountsCalculator.definedBenefit
    val annualAllowance = amountsCalculator.annualAllowance
    val previous3YearsUnusedAllowance = me.previous3YearsUnusedAllowance
    if (definedBenefit >= annualAllowance) {
      (annualAllowance + previous3YearsUnusedAllowance - definedBenefit).max(0)
    } else {
      (me.unusedAllowance.min(MAX_CF) + previous3YearsUnusedAllowance).max(0)
    }
  }

  def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    val isTriggered = contribution.amounts.get.triggered.getOrElse(false)
    if (!isTriggered) {
      Some(SummaryResult(dbist=me.dbist))
    } else {
      Some(SummaryResult(me.chargableAmount, 
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
                         me.defaultChargableAmount))
    }
  }
}

object Year2015Period1Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long =
    PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period1Calculator")).getOrElse(80000L)

  def isSupported(contribution:Contribution):Boolean = {
    contribution.isPeriod1()
  }

  override def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    if (isSupported(contribution)) {
      val amountsCalculator: BasicAmountsCalculator = BasicAmountsCalculator(getAnnualAllowanceInPounds)
      if (contribution.isGroup1()) {
        // Period 1 only allows maximum carry forward of 40k (here in pence values)
        Group1P1Calculator(amountsCalculator).summary
      } else if (contribution.isGroup2) {
        Group2P1Calculator(amountsCalculator).summary
      } else None
    } else None
  }
}

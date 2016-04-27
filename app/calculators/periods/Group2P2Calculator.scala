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

/*case class Group2P2Calculator(amountsCalculator: BasicAmountsCalculator) extends calculators.Calculator {
  me => Group2P2Calculator
  val MPA = 10000*100L
  val AAA = 30000*100L
  val AA = 0L

  def period1UnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.filter(_.dbist > 0).headOption.map(_.unusedAllowance).getOrElse(0L)
  def previous2YearsUnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.filterNot(_.dbist > 0).slice(0,2).foldLeft(0L)(_+_.unusedAllowance)
  def previous3YearsUnusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.filterNot(_.dbist > 0).slice(0,3).foldLeft(0L)(_+_.unusedAllowance)
  def isMPAAApplicable(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Boolean = (isPeriod1Triggered || isPeriod2Triggered) && (previousPeriods.filter(_.dbist > 0).headOption.map(_.mpist > 0).getOrElse(false) && me.mpist > 0)
  def isPeriod1Triggered(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Boolean = previousPeriods.headOption.map(_.triggered).getOrElse(false)
  def isPeriod2Triggered(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Boolean = contribution.amounts.get.triggered.getOrElse(false)
  def preFlexiSavings(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = previousPeriods.filter(_.dbist > 0).foldLeft(0L)(_+_.dbist)
  def definedBenefit(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).definedBenefit.getOrElse(0L)
  def definedContribution(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)
  def moneyPurchaseAA(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = 0L
  def alternativeAA(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = AAA.min(30000*100L)
  def exceedingAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = (me.definedBenefit - me.annualAllowance).max(0)
  def annualAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = AA

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
    (amounts.moneyPurchase.getOrElse(0L)-MPA) // negative if MP is less than MPA
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val mpaa = previousPeriods.headOption.map(_.moneyPurchaseAA).getOrElse(0L)
    val p1aca = previousPeriods.headOption.map(_.alternativeChargableAmount).getOrElse(0L)
    val p1dca = previousPeriods.headOption.map(_.defaultChargableAmount).getOrElse(0L)
    if ((p1aca > 0 || p1dca > 0) && (AAA <= me.definedContribution)) {
      me.definedContribution
    } else if (me.definedContribution >= MPA) {
      me.definedContribution - MPA
    } else if (me.definedContribution >= mpaa) {
      me.definedContribution - mpaa
    } else {
      0L
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    if (me.isPeriod2Triggered) {
      val mp = previousPeriods.filter(_.mpist != 0).map(_.unusedAllowance).getOrElse(0L)
    } else {
      val aa = 4000000L + previousPeriods.headOption.map(_.availableAAWithCCF).getOrElse(0L)
      val p1aca = previousPeriods.headOption.map(_.alternativeChargableAmount).getOrElse(0L)
      val p1dca = previousPeriods.headOption.map(_.defaultChargableAmount).getOrElse(0L)
      if ((p1aca > 0 || p1dca > 0) && (AAA <= me.definedContribution)) {
        me.definedContribution - AAA
      } else if (aa > me.definedContribution) {
        0L
      } else {
        val unusedAAA = previousPeriods.headOption.map(_.moneyPurchaseAA).getOrElse(0L)
        me.definedContribution - (unusedAAA + MPA)
      }
    }
  }

  def unusedAllowance(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    if (!me.isPeriod1Triggered) {
      (4000000L - me.definedBenefit).max(0)
    } else {
      (previousPeriods.headOption.map(_.unusedAllowance).getOrElse(0L) - (me.definedBenefit+me.definedContribution)).max(0)
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
    if (me.period1UnusedAllowance <= 0 && me.definedContribution > AAA) {
      0L
    } else {
      me.period1UnusedAllowance
    }
  }

  def aaCCF(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Long = {
    val definedContribution = amountsCalculator.definedContribution
    val period1UnusedAllowance = me.period1UnusedAllowance
    if (MPA > me.definedContribution) {
      val period1MPAA = previousPeriods.filter(_.dbist != 0).headOption.map(_.moneyPurchaseAA).getOrElse(0L)
      if (period1MPAA > 0 ) {
        period1UnusedAllowance - (period1MPAA - definedContribution)
      } else {
        period1UnusedAllowance - (MPA - definedContribution)
      }
    } else {
      if (me.definedContribution >= AAA) {
        0
      } else {
        period1UnusedAllowance
      }
    }
  }

  def summary(implicit previousPeriods:Seq[SummaryResult], contribution: Contribution): Option[SummaryResult] = {
    Some(SummaryResult(me.chargableAmount, 
                       me.exceedingAllowance, 
                       me.annualAllowance, 
                       me.unusedAllowance, 
                       me.aaCF, 
                       me.aaCCF, 
                       0L,
                       Some(AdditionalFields(
                       me.moneyPurchaseAA,
                       me.alternativeAA,
                       me.dbist,
                       me.mpist,
                       me.alternativeChargableAmount,
                       me.defaultChargableAmount,
                       contribution.amounts.get.triggered.getOrElse(false)))))
  }
}
*/

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

case class Group2P2Calculator(amountsCalculator: BasicCalculator) extends PeriodCalculator {
  me: Group2P2Calculator => 

  val MPA = 10000 * 100L
  val AAA = 30000 * 100L

  def period1(implicit previousPeriods:Seq[TaxYearResults]) = {
    previousPeriods.headOption.map(_.summaryResult.asInstanceOf[Group2Fields]).getOrElse(Group2Fields())
  }

  def isMPAAApplicable(implicit contribution: Contribution): Boolean = {
    me.definedContribution > MPA
  }

  def definedBenefit(): Long = 0L

  def dbist(): Long = 0L

  def mpist(implicit contribution:Contribution): Long = me.definedContribution

  def moneyPurchaseAA(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    period1.unusedMPAA
  }

  def alternativeAA(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    period1.unusedAAA
  }

  def alternativeChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    if (me.isMPAAApplicable(contribution)) {
      (me.definedContribution - period1.unusedMPAA).max(0)
    } else {
      0L
    }
  }

  def defaultChargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val savings = me.mpist
    val period1ACA = period1.alternativeChargableAmount
    val period1AA = period1.unusedAllowance
    val period1AAA = period1.unusedAAA

    if (period1AAA > 0) {
      if (period1AAA > savings) {
        0L
      } else {
        (savings - (period1AAA + period1.availableAAWithCCF)).max(0)
      }
    } else {
      if (period1AA > savings) {
        0L
      } else {
        (savings - (period1AA + period1.availableAAWithCCF)).max(0)
      }
    }
  }

  def exceedingAllowance(): Long = 0L

  def annualAllowance(implicit previousPeriods:Seq[TaxYearResults]): Long = period1.unusedAllowance

  def unusedAllowance(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Long = {
    val period1Amounts = previousPeriods.headOption.map(_.input.amounts.get).getOrElse(InputAmounts())
    val period1DC = period1Amounts.moneyPurchase.getOrElse(0L)
    val period2DC = me.definedContribution
    if (period1.unusedAAA > 0) {
      0L
    } else {
      if (period1DC < 2000000L && period2DC < MPA){
        (me.annualAllowance - me.definedContribution).max(0)
      } else {
        me.annualAllowance
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

  def aaCF(implicit previousPeriods:Seq[TaxYearResults]): Long = period1.availableAAWithCCF

  def aaCCF(implicit previousPeriods: Seq[TaxYearResults], contribution:Contribution): Long = {
    val unused = me.unusedAllowance
    if (unused > 0) {
      (me.unusedAllowance + me.previous2YearsUnusedAllowance)
    } else {
      (me.previous3YearsUnusedAllowance - me.definedContribution).max(0)
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
                        me.unusedMPAA))
    }
  }
}

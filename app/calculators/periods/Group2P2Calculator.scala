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

case class Group2P2Calculator(implicit amountsCalculator: BasicCalculator,
                              previousPeriods:Seq[TaxYearResults], 
                              contribution:Contribution) extends PeriodCalculator {
  val group1P2Calculator = Group1P2Calculator()
  val MPA = 10000 * 100L
  val AAA = 30000 * 100L

  def period1() = {
    previousPeriods.headOption.map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
  }

  override def isMPAAApplicable(): Boolean = {
    definedContribution > MPA
  }

  override def definedBenefit(): Long = 0L

  override def dbist(): Long = 0L

  override def mpist(): Long = definedContribution

  override def moneyPurchaseAA(): Long = if (contribution.isTriggered) period1.unusedMPAA else 0L


  override def alternativeAA(): Long = if (contribution.isTriggered) period1.unusedAAA else 0L


  override def alternativeChargableAmount(): Long = {
    if (isMPAAApplicable) {
      (definedContribution - period1.unusedMPAA).max(0)
    } else {
      0L
    }
  }

  override def defaultChargableAmount(): Long = {
    val savings = mpist
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

  override def exceedingAllowance(): Long = if (contribution.isTriggered) 0L else group1P2Calculator.exceedingAllowance

  override def annualAllowance(): Long = period1.unusedAllowance

  override def unusedAllowance(): Long = {
    if (contribution.isTriggered) {
      val period1Amounts = previousPeriods.headOption.map(_.input.amounts.get).getOrElse(InputAmounts())
      val period1DC = period1Amounts.moneyPurchase.getOrElse(0L)
      val period2DC = definedContribution
      if (period1.unusedAAA > 0) {
        0L
      } else {
        if (period1DC < 2000000L && period2DC < MPA){
          (annualAllowance - definedContribution).max(0)
        } else {
          annualAllowance
        }
      }
    } else {
      group1P2Calculator.unusedAllowance
    }
  }

  override def chargableAmount(): Long = {
    if (contribution.isTriggered) {
      val dca = defaultChargableAmount
      if (isMPAAApplicable) {
        val aca = alternativeChargableAmount
        aca.max(dca) // if aca == dca then choose dca
      } else {
        dca
      }
    } else {
      group1P2Calculator.chargableAmount
    }
  }

  override def aaCF(): Long = if (contribution.isTriggered) period1.availableAAWithCCF else group1P2Calculator.aaCF

  override def aaCCF(): Long = {
    if (contribution.isTriggered) {
      if (unusedAllowance > 0) {
        (unusedAllowance + previous2YearsUnusedAllowance)
      } else {
        (previous3YearsUnusedAllowance - definedContribution).max(0)
      }
    } else {
      group1P2Calculator.aaCCF
    }
  }

  override def cumulativeMP(): Long = definedContribution + period1.cumulativeMP

  override def cumulativeDB(): Long = definedBenefit + period1.cumulativeDB

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      definedContribution - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = 0L

  override def unusedAAA(): Long = if (contribution.isTriggered) period1.unusedAAA else 0L

  override def unusedMPAA(): Long = 0L
}

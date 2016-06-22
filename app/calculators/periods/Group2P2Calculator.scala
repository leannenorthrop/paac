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
  val MPA = 10000 * 100L
  val P1MPA = 20000 * 100L
  val AAA = 30000 * 100L

  def basicCalculator(): BasicCalculator = amountsCalculator
  
  def period1or2 = previous.asInstanceOf[ExtendedSummaryFields]

  override def isMPAAApplicable(): Boolean = (definedContribution > MPA) || period1.isMPA || period1Amounts.moneyPurchase.getOrElse(0L) >= P1MPA

  override def definedBenefit(): Long = if (contribution.isGroup2) 0L else basicCalculator().definedBenefit

  override def dbist(): Long = 0L

  override def mpist(): Long = definedContribution

  override def moneyPurchaseAA(): Long = if (contribution.isTriggered) period1or2.unusedMPAA else 0L

  override def alternativeAA(): Long = if (contribution.isTriggered) period1or2.unusedAAA else 0L


  override def alternativeChargableAmount(): Long = {
    if (isMPAAApplicable) {
      (definedContribution - period1or2.unusedMPAA).max(0)
    } else {
      0L
    }
  }

  override def defaultChargableAmount(): Long = {
    if (contribution.isGroup2) {
      val savings = mpist
      val period1ACA = period1or2.alternativeChargableAmount
      val period1AA = period1or2.unusedAllowance
      val period1AAA = period1or2.unusedAAA
      if (period1AAA > 0) {
        if (period1AAA > savings) {
          0L
        } else {
          (savings - (period1AAA + period1or2.availableAAWithCCF)).max(0)
        }
      } else {
        if (period1AA > savings) {
          0L
        } else {
          (savings - (period1AA + period1or2.availableAAWithCCF)).max(0)
        }
      }
    } else {
      0L
    }
  }

  override def exceedingAllowance(): Long = if (contribution.isGroup2 && isTriggered) 0L else (basicCalculator().definedBenefit - period1.unusedAllowance).max(0)

  override def annualAllowance(): Long = if (contribution.isGroup2) period1or2.unusedAllowance else basicCalculator().annualAllowance

  override def unusedAllowance(): Long = {
    if (isTriggered) {
      val period1DC = previousInputs.moneyPurchase.getOrElse(0L)
      val period2DC = definedContribution
      if (period1or2.unusedAAA > 0) {
        0L
      } else {
        if (period1DC < P1MPA && period2DC < MPA){
          (annualAllowance - definedContribution).max(0)
        } else {
          annualAllowance
        }
      }
    } else {
      (period1.unusedAllowance - basicCalculator().definedBenefit).max(0)
    }
  }

  override def chargableAmount(): Long = {
    if (isTriggered) {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    } else {
      val cf = previous.availableAAWithCCF
      if (basicCalculator().definedBenefit > cf) basicCalculator().definedBenefit - cf else 0L
    }
  }

  override def aaCF(): Long = if (isTriggered) period1or2.availableAAWithCCF else previousResults.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  override def aaCCF(): Long = {
    if (isTriggered) {
      val unused = unusedAllowance
      if (period1or2.unusedAAA > 0) {
        previous2YearsUnusedAllowance + period1.availableAAWithCCF
      } else {
        if (unused > 0) {
          unused + previous2YearsUnusedAllowance
        } else {
          val ccf = (previous2YearsUnusedAllowance - period1NotTriggered.map(_.exceedingAAAmount).getOrElse(0L)).max(0L)
          if (ccf > previous.availableAAWithCCF) 0L else ccf
        }
      }
    } else {
      val execeeding = exceedingAllowance
        if (execeeding > 0) {
        val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())

          val period1AACCF = previousResults.availableAAWithCCF
          if (execeeding >= period1AACCF) {
            0L
          } else {
            val unusedAllowanceList = actualUnused.slice(0, 3).map(_._2)
            unusedAllowanceList.foldLeft(0L)(_ + _)
          }
        } else {
          val unusedAllowanceList = actualUnused.slice(0, 3).map(_._2)
          unusedAllowanceList.foldLeft(0L)(_ + _)
      }
    }
  }

  override def cumulativeMP(): Long = definedContribution + period1or2.cumulativeMP

  override def cumulativeDB(): Long = definedBenefit + period1or2.cumulativeDB

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      definedContribution - MPA
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = 0L

  override def unusedAAA(): Long = if (isTriggered) period1or2.unusedAAA.max(0) else 0L

  override def unusedMPAA(): Long = 0L
}

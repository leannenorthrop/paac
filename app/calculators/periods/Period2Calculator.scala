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

case class Period2Calculator(implicit amountsCalculator: BasicCalculator,
                                      previousPeriods:Seq[TaxYearResults], 
                                      contribution:Contribution) extends PeriodCalculator {
  val MPA = 10000 * 100L
  val P1MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 30000 * 100L
  val MAXAACF = 40000 * 100L

  override def aaCF(): Long = if (isTriggered && isGroup3) period1.availableAAWithCCF else previous.availableAAWithCCF 

  override def aaCCF(): Long = {
    if (!isTriggered) {
      actualUnused.slice(0, 3).map(_._2).foldLeft(0L)(_ + _)
    } else {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3)
          previous2YearsUnusedAllowance + period1.availableAAWithCCF - definedBenefit
        else
          previous2YearsUnusedAllowance + period1.availableAAWithCCF
      } else {
        if (unusedAllowance > 0) {
          previous2YearsUnusedAllowance + unusedAllowance
        } else {
          val exceedingAAAmount = preTriggerFields.map(_.exceedingAAAmount).getOrElse(0L)
          val ccf = (previous2YearsUnusedAllowance - exceedingAAAmount).max(0L)
          if (ccf > previous.availableAAWithCCF) 0L else ccf
        }
      }
    }
  }

  override def alternativeAA(): Long = if (isGroup3 || (isGroup2 && isTriggered)) previous.unusedAAA else 0L

  override def alternativeChargableAmount(): Long = {
    if (isGroup3 && (isMPAAApplicable || (isPeriod1Triggered && period1.isMPA))) 
      (mpist + dbist).max(0) 
    else if (isGroup2)
      if (isMPAAApplicable) {
        if (isPeriod1Triggered){ 
          (definedContribution - previous.unusedMPAA).max(0) 
        } else { 
          definedContribution - P2MPA
        }
      } else {
        if (previous.unusedMPAA < definedContribution) {
          (definedContribution - previous.unusedMPAA).max(0)
        } else {
          0L
        }
      }
    else { 0L }
  }

  override def annualAllowance(): Long = previous.unusedAllowance

  def basicCalculator(): BasicCalculator = amountsCalculator

  override def chargableAmount(): Long = {
    if (isTriggered) {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    } else {
      (basicDefinedBenefit - previous.availableAAWithCCF).max(0L)
    }
  }

  override def cumulativeDB(): Long = definedBenefit + previous.cumulativeDB

  override def cumulativeMP(): Long = definedContribution + previous.cumulativeMP

  override def dbist(): Long = {
    if (isGroup3)
      if (isPeriod1Triggered) {
        (definedBenefit - (preTriggerFields.get.unusedAAA + period1.availableAAWithCCF)).max(0)
      } else {
        (preFlexiSavings - period1.availableAAWithCCF).max(0)
      }
    else
      0L
  }

  override def defaultChargableAmount(): Long = {
    if (isGroup3 && isTriggered) {
      if (isPeriod1Triggered) {
        if (period1.isMPA) {
          /*
            // scenario 26 only 
            val savings = previous.preFlexiSavings + previous.postFlexiSavings
            val aacf = previous.availableAAWithCF
            postFlexiSavings - previous.dcaCF
          */
          ((definedContribution + definedBenefit) - (previous3YearsUnusedAllowance + period1.unusedAAA)).max(0)
        } else {
          ((definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      } else {
        ((preFlexiSavings + definedContribution) - period1.availableAAWithCCF).max(0)
      }
    } else if (isGroup2 && isTriggered) {
      if (previous.unusedAAA > 0) {
        (mpist - (previous.unusedAAA + previous.availableAAWithCCF)).max(0)
      } else {
        if (isPeriod1Triggered) {
          (mpist - (previous.unusedAllowance + previous.availableAAWithCCF)).max(0)
        } else {
          ((mpist + previous.mpist) - (previous.unusedAllowance + previous.availableAAWithCCF)).max(0)
        }
      }
    } else {
      0L
    }
  }

  def basicDefinedBenefit(): Long = basicCalculator().definedBenefit

  override def definedBenefit(): Long = {
    if (isGroup3) {
      if (isPeriod2Triggered) {
        previous.cumulativeDB
      } else {
        basicDefinedBenefit
      }
    } else if (isGroup2) 0L // definition of group 2 is that there is no db
      else basicDefinedBenefit
  }

  override def exceedingAAA(): Long = 0L

  override def exceedingAllowance(): Long = if ((isGroup2 || isGroup3) && isTriggered) 0L else (basicDefinedBenefit - period1.unusedAllowance).max(0)

  override def exceedingMPAA(): Long = if (isMPAAApplicable) (definedContribution - MPA).max(0) else 0L

  def isGroup1(implicit contribution: Contribution): Boolean = contribution.isGroup1
  def isGroup2(implicit contribution: Contribution): Boolean = !contribution.isGroup3 && contribution.isGroup2
  def isGroup3(implicit contribution: Contribution): Boolean = contribution.isGroup3

  override def isMPAAApplicable(): Boolean = if (isGroup3 || isGroup2)
                                               (definedContribution > MPA) || period1.isMPA || period1.cumulativeMP >= P1MPA || (previous.unusedMPAA < definedContribution)
                                             else false

  def isPeriod1Triggered(): Boolean = previousPeriods.find(taxResultTriggered).find(_.input.isPeriod1).isDefined

  def isPeriod2Triggered(): Boolean = isTriggered && !isPeriod1Triggered

  override def moneyPurchaseAA(): Long = if (isGroup3) period1.unusedMPAA else if (isGroup2 && isTriggered) previous.unusedMPAA else 0L

  override def mpist(): Long = {
    if (isGroup3) {
      if (isPeriod1Triggered) {
        (definedContribution - period1.unusedMPAA).max(0)
      } else if (isTriggered) {
        if (isMPAAApplicable) {
          (definedContribution - MPA).max(0)
        } else {
          0L
        }
      } else {
        0L
      }
    } else
      definedContribution
  }

  def period1(): ExtendedSummaryFields = previousPeriods.find(_.input.isPeriod1).flatMap(maybeExtended(_)).getOrElse(ExtendedSummaryFields())

  override def preFlexiSavings() : Long = if (isPeriod2Triggered) preTriggerInputs.map((c)=>c.moneyPurchase+c.definedBenefit).getOrElse(0L) else 0L

  def previous(): ExtendedSummaryFields = previousPeriods.headOption.flatMap(maybeExtended(_)).getOrElse(ExtendedSummaryFields())
  
  def previous2YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], c: Contribution): Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val contribution = Contribution(c.taxPeriodStart, c.taxPeriodEnd, Some(InputAmounts(0L,0L)))
    val actualUnused = basicCalculator().actualUnused(previousPeriods.drop(1), contribution)
    val noOfRows = if (!previousPeriods.find(_.input.isPeriod1).isDefined) 1 else 2
    actualUnused.drop(noOfRows).slice(0,2).foldLeft(0L)(_+_._2)
  }

  override def postFlexiSavings() : Long = if (isTriggered) definedContribution + definedBenefit else 0L

  override def unusedAAA(): Long = if (isTriggered) if (isGroup3) (period1.unusedAAA - contribution.definedBenefit).max(0) else previous.unusedAAA.max(0) else 0L

  override def unusedAllowance(): Long = {
    if (isTriggered) {
      if (isGroup3) {
        if (previous.unusedAAA > 0) {
          0L
        } else {
          val unusedAllowance = if (period1.isMPA) {
            period1.unusedAAA - definedBenefit
          } else {
            val previousSavings = previousPeriods.headOption.map(_.input).getOrElse(Contribution(0,0))
            val allSavings = definedBenefit + definedContribution + previousSavings.definedBenefit + previousSavings.moneyPurchase
            val unused = if (isPeriod1Triggered) {
              period1.unusedAllowance - contribution.definedBenefit
            } else {
              period1.unusedAllowance - (previousSavings.definedBenefit + previousSavings.moneyPurchase)
            }
            val savings = preFlexiSavings + (if (defaultChargableAmount >= alternativeChargableAmount) allSavings else 0L)
            if (savings > MAXAACF) 0L else unused
          }
          unusedAllowance.max(0)
        }
      } else { //if (contribution.isGroup2) {
        if (previous.unusedAAA > 0) {
          0L
        } else {
          if (period1.cumulativeMP < P1MPA && definedContribution < MPA){
            (annualAllowance - definedContribution).max(0)
          } else {
            annualAllowance
          }
        }
      }
    } else {
      (period1.unusedAllowance - basicDefinedBenefit).max(0)
    }
  }

  override def unusedMPAA(): Long = 0
}
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
  val AAA = 30000 * 100L
  val MAXAACF = 40000 * 100L

  override def aaCF(): Long = if (isTriggered && contribution.isGroup3) period1.availableAAWithCCF else previous.availableAAWithCCF 

  override def aaCCF(): Long = {
    if (isTriggered) {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3) 
          previous2YearsUnusedAllowance + period1.availableAAWithCCF - definedBenefit
        else 
          previous2YearsUnusedAllowance + period1.availableAAWithCCF
      } else {
        if (unusedAllowance > 0) {
          previous2YearsUnusedAllowance + unusedAllowance
        } else {
          val period1NotTriggered = previousPeriods.filter(taxResultNotTriggered).find(_.input.isPeriod1).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields])
          val ccf = (previous2YearsUnusedAllowance - period1NotTriggered.map(_.exceedingAAAmount).getOrElse(0L)).max(0L)
          if (ccf > previous.availableAAWithCCF) 0L else ccf
        }
      }
    } else {
        if (exceedingAllowance > 0) {
          if (exceedingAllowance >= previous.availableAAWithCCF) 0L else actualUnused.slice(0, 3).map(_._2).foldLeft(0L)(_ + _)
        } else {
          actualUnused.slice(0, 3).map(_._2).foldLeft(0L)(_ + _)
      }
    }
  }

  override def alternativeAA(): Long = if (contribution.isGroup3 || (contribution.isGroup2 && isTriggered)) previous.unusedAAA else 0L

  override def alternativeChargableAmount(): Long = if (contribution.isGroup3 && (isMPAAApplicable || (isPeriod1Triggered && period1.isMPA))) (mpist + dbist).max(0) 
                                                    else if (contribution.isGroup2 && isMPAAApplicable) (definedContribution - previous.unusedMPAA).max(0) 
                                                    else 0L

  override def annualAllowance(): Long = if (contribution.isGroup3) period1.unusedAllowance 
                                         else if (contribution.isGroup2) previous.unusedAllowance 
                                         else basicCalculator().annualAllowance

  def basicCalculator(): BasicCalculator = amountsCalculator

  override def chargableAmount(): Long = {
    if (isTriggered) {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    } else {
      (basicCalculator().definedBenefit - previous.availableAAWithCCF).max(0L)
    }
  }

  override def cumulativeDB(): Long = definedBenefit + previous.cumulativeDB

  override def cumulativeMP(): Long = if (contribution.isGroup3) definedContribution + period1.cumulativeMP else definedContribution + previous.cumulativeMP

  override def dbist(): Long = {
    if (contribution.isGroup3)
      if (isPeriod1Triggered) {
        (definedBenefit - (preTriggerFields.get.unusedAAA + period1.availableAAWithCCF)).max(0)
      } else {
        (period2PreTriggerSavings - period1.availableAAWithCCF).max(0)
      }
    else
      0L
  }

  override def defaultChargableAmount(): Long = {
    if (contribution.isGroup3 && isTriggered) {
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
        ((period2PreTriggerSavings + definedContribution) - period1.availableAAWithCCF).max(0)
      }
    } else if (contribution.isGroup2) {
      if (previous.unusedAAA > 0) {
        (mpist - (previous.unusedAAA + previous.availableAAWithCCF)).max(0)
      } else {
        (mpist - (previous.unusedAllowance + previous.availableAAWithCCF)).max(0)
      }
    } else {
      0L
    }
  }

  override def definedBenefit(): Long = {
    if (contribution.isGroup3) {
      if (isPeriod2Triggered) {
        previous.cumulativeDB
      } else {
        basicCalculator().definedBenefit
      }
    } else if (contribution.isGroup2) 0L // definition of group 2 is that there is no db
      else basicCalculator().definedBenefit
  }

  override def exceedingAAA(): Long = 0L

  override def exceedingAllowance(): Long = if ((contribution.isGroup2 || contribution.isGroup3) && isTriggered) 0L else (basicCalculator().definedBenefit - period1.unusedAllowance).max(0)

  override def exceedingMPAA(): Long = if (isMPAAApplicable) definedContribution - MPA else 0L

  override def isMPAAApplicable(): Boolean = if (contribution.isGroup3 || contribution.isGroup2)
                                               (definedContribution > MPA) || period1.isMPA || period1.cumulativeMP >= P1MPA
                                             else false

  def isPeriod1Triggered(): Boolean = previousPeriods.find(taxResultTriggered).find(_.input.isPeriod1).isDefined

  def isPeriod2Triggered(): Boolean = isTriggered && !isPeriod1Triggered

  override def moneyPurchaseAA(): Long = if (contribution.isGroup3) period1.unusedMPAA else if (contribution.isGroup2 && isTriggered) previous.unusedMPAA else 0L

  override def mpist(): Long = {
    if (contribution.isGroup3) {
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

  def period1(): ExtendedSummaryFields = previousPeriods.find(_.input.isPeriod1).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())

  def period2PreTriggerSavings(): Long = if (isPeriod2Triggered) preTriggerInputs.map((c)=>c.moneyPurchase+c.definedBenefit).getOrElse(0L) else 0L

  override def preFlexiSavings() : Long = if (isTriggered) period2PreTriggerSavings() else definedContribution + definedBenefit

  def previous(): ExtendedSummaryFields = previousPeriods.headOption.map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
  
  def previous2YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], c: Contribution): Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val contribution = Contribution(c.taxPeriodStart, c.taxPeriodEnd, Some(InputAmounts(0L,0L)))
    val actualUnused = basicCalculator().actualUnused(previousPeriods.drop(1), contribution)
    val noOfRows = if (!previousPeriods.find(_.input.isPeriod1).isDefined) 1 else 2
    actualUnused.drop(noOfRows).slice(0,2).foldLeft(0L)(_+_._2)
  }

  override def postFlexiSavings() : Long = if (isTriggered) definedContribution + definedBenefit else 0L

  override def unusedAAA(): Long = if (isTriggered) if (contribution.isGroup3) (period1.unusedAAA - contribution.definedBenefit).max(0) else previous.unusedAAA.max(0) else 0L

  override def unusedAllowance(): Long = {
    if (isTriggered) {
      if (contribution.isGroup3) {
        if (previous.unusedAAA > 0) {
          0L
        } else {
          val unusedAllowance = if (period1.isMPA) {
            period1.unusedAAA - definedBenefit
          } else {
            val delta = if (defaultChargableAmount >= alternativeChargableAmount) {
              definedBenefit + definedContribution + (if (isPeriod2Triggered) 0L else period1.cumulativeDB)
            } else 0L

            val deduct = if (isPeriod1Triggered) contribution.definedBenefit else period2PreTriggerSavings
            if (period2PreTriggerSavings + delta > MAXAACF) 0L else period1.unusedAllowance - deduct
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
      (period1.unusedAllowance - basicCalculator().definedBenefit).max(0)
    }
  }

  override def unusedMPAA(): Long = 0L
}
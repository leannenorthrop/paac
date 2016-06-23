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

  override def aaCF(): Long = if (isTriggered) 
                                if (contribution.isGroup3) period1.availableAAWithCCF 
                                else previous.availableAAWithCCF 
                              else previous.availableAAWithCCF

  override def aaCCF(): Long = {
    if (isTriggered) {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3) 
          previous2YearsUnusedAllowance + period1.availableAAWithCCF - definedBenefit
        else 
          previous2YearsUnusedAllowance + period1.availableAAWithCCF
      } else {
        if (unusedAllowance > 0) {
          unusedAllowance + previous2YearsUnusedAllowance
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

  override def alternativeAA(): Long = if (contribution.isGroup3) period1.unusedAAA 
                                       else if (contribution.isGroup2 && contribution.isTriggered) previous.unusedAAA 
                                       else 0L

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
      val cf = previous.availableAAWithCCF
      if (basicCalculator().definedBenefit > cf) basicCalculator().definedBenefit - cf else 0L
    }
  }

  override def cumulativeDB(): Long = if (contribution.isGroup3) definedBenefit + period1.cumulativeDB 
                                      else definedBenefit + previous.cumulativeDB

  override def cumulativeMP(): Long = if (contribution.isGroup3) flexiAccessSavings + period1.cumulativeMP 
                                      else definedContribution + previous.cumulativeMP

  override def dbist(): Long = {
    if (contribution.isGroup3)
      if (isPeriod1Triggered) {
        val allowances = (preTriggerFields.get.unusedAAA + period1.availableAAWithCCF)
        if (definedBenefit < allowances) {
          0L
        } else {
          (definedBenefit - allowances).max(0)
        }
      } else {
        (period2PreTriggerSavings - period1.availableAAWithCCF).max(0)
      }
    else
      0L
  }

  override def defaultChargableAmount(): Long = {
    if (contribution.isGroup3 && contribution.isTriggered) {
      if (isPeriod1Triggered) {
        if (period1.isMPA) {
          /*
            // scenario 26 only 
            val savings = previous.preFlexiSavings + previous.postFlexiSavings
            val aacf = previous.availableAAWithCF
            postFlexiSavings - previous.dcaCF
          */
          ((flexiAccessSavings + definedBenefit) - (previous3YearsUnusedAllowance + period1.unusedAAA)).max(0)
        } else {
          ((flexiAccessSavings + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      } else {
        ((period2PreTriggerSavings + flexiAccessSavings) - period1.availableAAWithCCF).max(0)
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
      if (!isPeriod1Triggered && isTriggered) {
        previous.cumulativeDB
      } else {
        basicCalculator().definedBenefit
      }
    } else if (contribution.isGroup2) 0L 
      else basicCalculator().definedBenefit
  }

  override def exceedingAAA(): Long = 0L

  override def exceedingAllowance(): Long = if ((contribution.isGroup2 || contribution.isGroup3) && isTriggered) 0L 
                                            else (basicCalculator().definedBenefit - period1.unusedAllowance).max(0)

  override def exceedingMPAA(): Long = if (isMPAAApplicable) 
                                        if (contribution.isGroup3) flexiAccessSavings - MPA 
                                        else definedContribution - MPA 
                                       else 0L

  def flexiAccessSavings(implicit contribution:Contribution): Long = contribution.moneyPurchase

  override def isMPAAApplicable(): Boolean = if (contribution.isGroup3 || contribution.isGroup2)
                                               (flexiAccessSavings > MPA) || period1.isMPA || period1.cumulativeMP >= P1MPA
                                             else false

  def isPeriod1Triggered(): Boolean = previousPeriods.find(taxResultTriggered).find(_.input.isPeriod1).isDefined

  override def moneyPurchaseAA(): Long = if (contribution.isGroup3) period1.unusedMPAA else if (contribution.isGroup2 && contribution.isTriggered) previous.unusedMPAA else 0L

  override def mpist(): Long = {
    if (contribution.isGroup3) {
      if (isPeriod1Triggered) {
        (flexiAccessSavings - period1.unusedMPAA).max(0)
      } else if (contribution.isTriggered) {
        if (isMPAAApplicable) {
          (flexiAccessSavings - MPA).max(0)
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

  def period2PreTriggerSavings(): Long = if (isTriggered && !isPeriod1Triggered) preTriggerInputs.map((c)=>c.moneyPurchase+c.definedBenefit).getOrElse(0L) else 0L

  def postTriggerSavings(): Long = if (!isPeriod1Triggered && isTriggered) definedBenefit + definedContribution else definedBenefit + definedContribution + period1.cumulativeDB

  override def preFlexiSavings() : Long = if (isTriggered) period2PreTriggerSavings() else definedContribution + definedBenefit

  def preTriggerAmounts(): Option[InputAmounts] = previousPeriods.find(taxResultNotTriggered).flatMap(_.input.amounts)

  def preTriggerSavings(): Long = if (!isPeriod1Triggered && isTriggered) period1.cumulativeDB + period2PreTriggerSavings else period2PreTriggerSavings

  def previous(): ExtendedSummaryFields = previousPeriods.headOption.map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
  
  def previous2YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], c: Contribution): Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val contribution = Contribution(c.taxPeriodStart, c.taxPeriodEnd, Some(InputAmounts(0L,0L)))

    val l = if (!previousPeriods.find(_.input.isPeriod1).isDefined) {
      val v = basicCalculator().actualUnused(previousPeriods.drop(1), contribution).drop(1).slice(0,2)
      v
    } else {
      val v = basicCalculator().actualUnused(previousPeriods.drop(1), contribution).drop(2).slice(0,2)
      v
    }

    l.foldLeft(0L)(_+_._2)
  }

  override def postFlexiSavings() : Long = if (isTriggered) flexiAccessSavings + definedBenefit else 0L

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
            val savings = if (defaultChargableAmount >= alternativeChargableAmount) {
              period2PreTriggerSavings + postTriggerSavings
            } else {
              period2PreTriggerSavings
            }
            val deduct = if (isPeriod1Triggered) contribution.definedBenefit else period2PreTriggerSavings
            if (savings > MAXAACF) 0L else period1.unusedAllowance - deduct
          }
          unusedAllowance.max(0)
        }
      } else { //if (contribution.isGroup2) {
        val period1DC = period1.cumulativeMP
        val period2DC = definedContribution
        if (previous.unusedAAA > 0) {
          0L
        } else {
          if (period1DC < P1MPA && period2DC < MPA){
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

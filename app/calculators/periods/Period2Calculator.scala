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

  def basicCalculator(): BasicCalculator = amountsCalculator
  
  def preTriggerAmounts(implicit previousPeriods:Seq[TaxYearResults]): Option[InputAmounts] = previousPeriods.find(taxResultNotTriggered).flatMap(_.input.amounts)

  def previousInputs(implicit previousPeriods:Seq[TaxYearResults]): InputAmounts = previousPeriods.headOption.map(_.input.amounts.getOrElse(InputAmounts())).getOrElse(InputAmounts())

  def period1or2 = previous.asInstanceOf[ExtendedSummaryFields]

  def previous(implicit previousPeriods:Seq[TaxYearResults]): Summary = previousPeriods.headOption.map(_.summaryResult).getOrElse(ExtendedSummaryFields())

  def period1Amounts(implicit previousPeriods:Seq[TaxYearResults]): InputAmounts = previousPeriods.find(_.input.isPeriod1).map(_.input.amounts.getOrElse(InputAmounts())).getOrElse(InputAmounts()) 

  def period1Triggered(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = previousPeriods.find(taxResultTriggered).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields])

  def period1NotTriggered(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = previousPeriods.filter(taxResultNotTriggered).find(_.input.isPeriod1).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields])

  def isPeriod1Triggered(implicit previousPeriods:Seq[TaxYearResults]): Boolean = {
    previousPeriods.find(_.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)).isDefined
  }

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

  def sum(values: List[Option[Long]]): Long = values.map(_.getOrElse(0L)).foldLeft(0L)(_+_)

  def preTriggerSavings(): Long = {
    val values = if (!isPeriod1Triggered && isTriggered) {
      val p2BeforeTrigger = preTriggerAmounts.getOrElse(InputAmounts())
      List(period1Amounts,p2BeforeTrigger).flatMap((v)=>List(v.moneyPurchase, v.definedBenefit))
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      List(amounts.moneyPurchase,amounts.definedBenefit)
    }
    sum(values)
  }

  def postTriggerSavings(): Long = {
    if (!isPeriod1Triggered && isTriggered) {
      definedBenefit + definedContribution
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      definedBenefit + definedContribution + period1Amounts.definedBenefit.getOrElse(0L) + period1Amounts.moneyPurchase.getOrElse(0L)
    }
  }

  def period2PreTriggerSavings(): Long = {
    if (isPeriod1Triggered) {
      0L
    } else {
      val amounts = preTriggerAmounts.getOrElse(InputAmounts())
      sum(List(amounts.moneyPurchase,amounts.definedBenefit))
    }
  }

  def flexiAccessSavings(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  override def isMPAAApplicable(): Boolean = {
    if (contribution.isGroup3)
      (flexiAccessSavings > MPA) || period1.isMPA || period1Amounts.moneyPurchase.getOrElse(0L) >= P1MPA
    else if (contribution.isGroup2)
      (definedContribution > MPA) || period1.isMPA || period1Amounts.moneyPurchase.getOrElse(0L) >= P1MPA
    else 
      false
  }

  override def definedBenefit(): Long = {
    if (contribution.isGroup3) {
      if (!isPeriod1Triggered && isTriggered) {
        previousInputs.definedBenefit.getOrElse(0L)
      } else {
        contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)
      }
    } else if (contribution.isGroup2) 0L 
      else basicCalculator().definedBenefit
  }

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

  override def mpist(): Long = {
    if (contribution.isGroup3) {
      if (isPeriod1Triggered) {
        (flexiAccessSavings - period1Triggered.get.unusedMPAA).max(0)
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

  override def moneyPurchaseAA(): Long = if (contribution.isGroup3) period1.unusedMPAA else if (contribution.isGroup2 && contribution.isTriggered) period1or2.unusedMPAA else 0L

  override def alternativeAA(): Long = if (contribution.isGroup3) period1.unusedAAA else if (contribution.isGroup2 && contribution.isTriggered) period1or2.unusedAAA else 0L


  override def alternativeChargableAmount(): Long = {
    if (contribution.isGroup3 && (isMPAAApplicable || (isPeriod1Triggered && period1Triggered.get.isMPA))) {
      (mpist + dbist).max(0)
    }
    else if (contribution.isGroup2 && isMPAAApplicable) {
      (definedContribution - period1or2.unusedMPAA).max(0)
    } else {
      0L
    }
  }

  override def defaultChargableAmount(): Long = {
    if (contribution.isGroup3 && contribution.isTriggered) {
      period1Triggered.map {
        (fields) =>
        if (fields.isMPA) {
          val previous = previousPeriods.headOption.map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())
          /*
            // scenario 26 only 
            val savings = previous.preFlexiSavings + previous.postFlexiSavings
            val aacf = previous.availableAAWithCF
            postFlexiSavings - previous.dcaCF
          */
          ((flexiAccessSavings + definedBenefit) - (previous3YearsUnusedAllowance + fields.unusedAAA)).max(0)
        } else {
          ((flexiAccessSavings + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      }.getOrElse {
        ((period2PreTriggerSavings + flexiAccessSavings) - period1.availableAAWithCCF).max(0)
      }
    } else if (contribution.isGroup2) {
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

  override def exceedingAllowance(): Long = if ((contribution.isGroup2 || contribution.isGroup3) && isTriggered) 0L else (basicCalculator().definedBenefit - period1.unusedAllowance).max(0)

  override def annualAllowance(): Long = if (contribution.isGroup3) period1.unusedAllowance else if (contribution.isGroup2) period1or2.unusedAllowance else basicCalculator().annualAllowance

  def p2definedBenefit(): Long = contribution.amounts.map(_.definedBenefit.getOrElse(0L)).getOrElse(0L)

  def period1(implicit previousPeriods:Seq[TaxYearResults]): ExtendedSummaryFields = previousPeriods.find(_.input.isPeriod1).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields]).getOrElse(ExtendedSummaryFields())

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
            val deduct = if (isPeriod1Triggered) p2definedBenefit else period2PreTriggerSavings
            if (savings > MAXAACF) 0L else period1.unusedAllowance - deduct
          }
          unusedAllowance.max(0)
        }
      } else { //if (contribution.isGroup2) {
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

  override def aaCF(): Long = if (isTriggered) if (contribution.isGroup3) period1.availableAAWithCCF else period1or2.availableAAWithCCF else previousPeriods.headOption.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  override def aaCCF(): Long = {
    if (isTriggered) {
      val unused = unusedAllowance
      if (period1or2.unusedAAA > 0) {
        if (contribution.isGroup3) 
          previous2YearsUnusedAllowance + period1.availableAAWithCCF - definedBenefit
        else 
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

  override def cumulativeMP(): Long = if (contribution.isGroup3) flexiAccessSavings + period1.cumulativeMP else definedContribution + period1or2.cumulativeMP

  override def cumulativeDB(): Long = if (contribution.isGroup3) definedBenefit + period1.cumulativeDB else definedBenefit + period1or2.cumulativeDB

  override def exceedingMPAA(): Long = {
    if (isMPAAApplicable) {
      if (contribution.isGroup3) {
        flexiAccessSavings - MPA
      } else {
        definedContribution - MPA
      }
    } else {
      0L
    }
  }

  override def exceedingAAA(): Long = 0L

  override def unusedAAA(): Long = if (isTriggered) if (contribution.isGroup3) (period1.unusedAAA - p2definedBenefit).max(0) else period1or2.unusedAAA.max(0) else 0L

  override def unusedMPAA(): Long = 0L

  override def preFlexiSavings() : Long = {
    if (isTriggered) {
      period2PreTriggerSavings()
    } else {
      definedContribution + definedBenefit
    }
  }

  override def postFlexiSavings() : Long = {
    if (isTriggered) {
      flexiAccessSavings + definedBenefit
    } else {
      0L
    }
  }
}

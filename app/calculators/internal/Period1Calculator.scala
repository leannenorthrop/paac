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

package calculators.internal

import models._
import calculators.internal.utilities._
import play.api.Logger

// scalastyle:off number.of.methods
trait Year2015Period1Calculator extends PeriodCalculator {
  base: PeriodCalculator =>

  Logger.debug(s"\n***************************** 2015 Period 1 ${contribution.amounts} *****************************")

  def allowanceInPounds(): Long
  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  val MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 60000 * 100L
  val P2AAA = 30000 * 100L
  val AA = 80000 * 100L
  val MAX_CF = 4000000L

  def allowance(): Long = allowanceInPounds

  // Annual Allowance Cumulative Carry Forwards
  protected lazy val _aaCCF =
    if (!isTriggered) {
      val v = actualUnused(this)(4)(previousPeriods,contribution) // scalastyle:ignore
      Logger.debug(s"AACCF (nte): ${v}")
      v
    }
    else if (isMPAAApplicable) {
      val aaa = ((AAA - preTriggerSavings).min(P2AAA) + _previous3YearsUnusedAllowance)
      Logger.debug(s"AACCF(mp1): (${AAA} - ${preTriggerSavings}).min(${P2AAA}) + ${_previous3YearsUnusedAllowance} = ${aaa}")
      (aaa).max(0)
    } else if (definedBenefit >= AA) {
      val v = (AA + _previous3YearsUnusedAllowance - postFlexiSavings).max(0)
      Logger.debug(s"AACCF(>): ${AA} + ${_previous3YearsUnusedAllowance} - ${postFlexiSavings} = ${v}")
      v
    } else {
      val v = (unusedAllowance.min(MAX_CF) + _previous3YearsUnusedAllowance).max(0)
      Logger.debug(s"AACCF(<): ${unusedAllowance.min(MAX_CF)} + ${_previous3YearsUnusedAllowance} = ${v}")
      v
    }
  override def annualAllowanceCCF(): Long = { Logger.debug(s"********************** annualAllowanceCCF() = ${_aaCCF}"); _aaCCF }

  override def availableAAAWithCCF(): Long = _aaCCF // same value as aaCCF considers AAA in calculation

  // Annual Allowance With Carry Forwards
  protected lazy val _aaCF = if (!isTriggered) {
                               annualAllowance + previous.availableAAWithCCF
                             } else {
                               previous.availableAAWithCF
                             }
  override def annualAllowanceCF(): Long = _aaCF

  override def availableAAAWithCF(): Long = if (!isTriggered) {
         0L
       } else {
         alternativeAA() + _previous3YearsUnusedAllowance
       }

  // Alternative Chargable Amount With Carry Forwards
  protected lazy val _acaCF = if (isTriggered) {
                                0L
                              } else {
                                (AAA + _previous3YearsUnusedAllowance) - preFlexiSavings
                              }
  override def acaCF() : Long = _acaCF

  // Alternative Annual Allowance
  protected lazy val _alternativeAA = if (isMPAAApplicable) {
                                        AAA
                                      } else {
                                        0L
                                      }
  override def alternativeAA(): Long = _alternativeAA

  // Alternative Chargable Amount
  protected lazy val _alternativeChargableAmount = if (isMPAAApplicable) {
                                                     mpist + dbist
                                                   } else {
                                                     0L
                                                   }
  override def alternativeChargableAmount(): Long = _alternativeChargableAmount

  // Annual Allowance
  protected lazy val _annualAllowance = if (!isTriggered) {
                                          AA
                                        } else if (defaultChargableAmount >= alternativeChargableAmount) {
                                          AA
                                        } else {
                                          AAA
                                        }
  override def annualAllowance(): Long = _annualAllowance

  def basicCalculator(): SummaryCalculator = BasicAllowanceCalculator(allowance, previousPeriods, contribution)

  // Chargable Amount (tax due)
  protected lazy val _chargableAmount = if (!isTriggered) {
                                          basicCalculator().chargableAmount
                                        } else if (isMPAAApplicable) {
                                          alternativeChargableAmount.max(defaultChargableAmount)
                                        } else {
                                          defaultChargableAmount
                                        }
  override def chargableAmount(): Long = _chargableAmount

  // Cumulative Defined Benefit
  protected lazy val _cumulativeDB = definedBenefit
  override def cumulativeDB(): Long = _cumulativeDB

  // Cumulative Money Purchase
  protected lazy val _cumulativeMP = definedContribution
  override def cumulativeMP(): Long = _cumulativeMP

  // DBIST
  protected lazy val _dbist = {
    val year2014CCF = previousPeriods.filter(isBefore2015).headOption.map(_.summaryResult).getOrElse(SummaryResult()).availableAAWithCCF

    if (isTriggered) {
      val unusedaaa = preTriggerFields(previousPeriods).map(_.unusedAAA).getOrElse(0L)
      val allowances = unusedaaa + year2014CCF
      if (definedBenefit < allowances) {
        0L
      } else {
        (allowances - definedBenefit).max(0)
      }
    } else {
      (preTriggerSavings - year2014CCF).max(0)
    }
  }
  override def dbist(): Long = _dbist

  // Default Chargable Amount With Carry Forwards
  protected lazy val _dcaCF = if (!isTriggered) {
                                0L
                              } else {
                                (AA + _previous3YearsUnusedAllowance) - postFlexiSavings
                              }
  override def dcaCF() : Long = _dcaCF

  // Defined Benefit
  // treat both money purchase and defined benefit as same prior to flexi access
  protected lazy val _definedBenefit = if (isTriggered) {
                                         contribution.definedBenefit + preTriggerSavings
                                       } else {
                                         contribution.definedBenefit + contribution.moneyPurchase
                                       }
  override def definedBenefit(): Long = _definedBenefit

  protected lazy val _definedContribution = basicCalculator.definedContribution
  def definedContribution(): Long = _definedContribution

  // Default Chargable Amount
  protected lazy val _defaultChargableAmount =
    if (!isTriggered) {
      0L
    } else {
      (postFlexiSavings - (AA + _previous3YearsUnusedAllowance)).max(0L)
    }
  override def defaultChargableAmount(): Long = _defaultChargableAmount

  // Exceeding Alternative Annual Allowance
  protected lazy val _exceedingAAA = if (isMPAAApplicable) {
                                       (definedBenefit - AAA).max(0)
                                     } else {
                                       0L
                                     }
  override def exceedingAAA(): Long = _exceedingAAA

  // Exceeding Annual Allowance
  protected lazy val _exceedingAllowance = if (isTriggered) {
                                            ((definedBenefit + definedContribution) - AA).max(0)
                                           } else {
                                             basicCalculator.exceedingAllowance
                                           }
  override def exceedingAllowance(): Long = _exceedingAllowance

  // Exceeding Money Purchase Allowance
  protected lazy val _exceedingMPAA = if (isMPAAApplicable) {
                                        definedContribution - MPA
                                      } else {
                                        0L
                                      }
  override def exceedingMPAA(): Long = _exceedingMPAA

  // Is ACA Applicable
  protected lazy val _isACA = isTriggered && alternativeChargableAmount > defaultChargableAmount
  override def isACA(): Boolean = _isACA

  // Is MPA Applicable
  protected lazy val _isMPAAApplicable = isTriggered && definedContribution > MPA
  override def isMPAAApplicable(): Boolean = _isMPAAApplicable

  def isTriggered(): Boolean = contribution.isTriggered

  // Money Purchase Annual Allowance
  override def moneyPurchaseAA(): Long = MPA

  // MPIST
  protected lazy val _mpist = if (isMPAAApplicable) {
                                definedContribution - MPA
                              } else {
                                definedContribution
                              }
  override def mpist(): Long = _mpist

  // Pre-Flexi Access Savings
  protected lazy val _preFlexiSavings = if (isTriggered) {
                                          preTriggerSavings
                                        } else {
                                          definedContribution + definedBenefit
                                        }
  override def preFlexiSavings() : Long = _preFlexiSavings

  protected lazy val _preTriggerSavings = preTriggerInputs(previousPeriods).map((c)=>c.definedBenefit + c.moneyPurchase).getOrElse(0L)
  def preTriggerSavings(): Long = _preTriggerSavings

  // Previous row results
  protected lazy val _previous = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())
  def previous(): Summary = _previous

  protected lazy val _previous3YearsUnusedAllowance = base.previous3YearsUnusedAllowance(previousPeriods)

  // Post Flexi Access Savings
  protected lazy val _postFlexiSavings = if (isTriggered) {
                                           definedContribution + definedBenefit
                                         } else {
                                           0L
                                         }
  override def postFlexiSavings() : Long = _postFlexiSavings

  // Unused Alternative Annual Allowance
  protected lazy val _unusedAAA = if (isMPAAApplicable) {
                                    val v = (AAA - definedBenefit).min(P2AAA).max(0)
                                    Logger.debug(s"unusedAAA(mpa): ${AAA} - ${definedBenefit}")
                                    v
                                  } else {
                                    0L
                                  }
  override def unusedAAA(): Long = _unusedAAA

  // Unused Annual Allowance
  protected lazy val _unusedAllowance =
    if (!isTriggered) {
      basicCalculator().unusedAllowance.min(MAX_CF)
    } else {
      if (isMPAAApplicable) {
        0L
      } else {
        val savings = if (defaultChargableAmount >= alternativeChargableAmount) {
                        preTriggerSavings + definedContribution
                      } else {
                        preTriggerSavings
                      }
        val unused = if (savings > AA) {
          0L
        } else {
          (AA - savings).min(MAX_CF)
        }
        unused.max(0)
      }
    }
  override def unusedAllowance(): Long = _unusedAllowance

  // Unused Money Purchase Annual Allowance
  protected lazy val _unusedMPAA = if (isTriggered && !isMPAAApplicable) {
                                     (MPA - definedContribution).min(P2MPA)
                                   } else {
                                     0L
                                   }
  override def unusedMPAA(): Long = _unusedMPAA

  Logger.debug(s"\n***************************** 2015 Period 1 (end) *****************************")
}
// scalastyle:on number.of.methods

protected case class Period1Calculator(implicit allowanceInPounds: Long,
                                                previousPeriods:Seq[TaxYearResults],
                                                contribution: Contribution) extends Year2015Period1Calculator {
}

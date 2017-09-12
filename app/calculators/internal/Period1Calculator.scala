/*
 * Copyright 2017 HM Revenue & Customs
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
import scala.util._

// scalastyle:off number.of.methods
trait Year2015Period1Calculator extends PeriodCalculator {
  base: PeriodCalculator =>

  def allowanceInPounds(): Long
  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  val MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 60000 * 100L
  val P2AAA = 30000 * 100L
  val AA = 80000 * 100L
  val MAX_CF = 4000000L

  protected lazy val year = 20151

  def allowance(): Long = allowanceInPounds

  // Annual Allowance Cumulative Carry Forwards
  protected lazy val _aaCCF =
    if (!isTriggered) {
      val v = actualUnused(this)(4)(previousPeriods,contribution) // scalastyle:ignore
      ccfdetails(YEAR, contribution, previousPeriods)(this, this)
      detail("allowance.ccf.calculation.reason","nte")
      v
    }
    else if (isMPAAApplicable) {
      val v = AAA - preTriggerSavings
      val aaa = (v).min(P2AAA) + _previous3YearsUnusedAllowance
      if (v < P2AAA) {
        ccfdetails(YEAR, contribution, previousPeriods)(this, this)
        detail("allowance.ccf.calculation",
               s"aaa:${currency(AAA)};op: - ;sep:(;" +
               detail("pts.calculation") + "sep:);op: + ;" +
               detail("allowance.ccf.calculation"))
      } else {
        ccfdetails(YEAR, contribution, previousPeriods)(this, this)
        detail("allowance.ccf.calculation",s"aaa:${currency(P2AAA)};op: + ;" + detail("allowance.ccf.calculation"))
      }
      detail("allowance.ccf.calculation.reason","mpa")
      (aaa).max(0)
    } else if (definedBenefit >= AA) {
      val v = (AA + _previous3YearsUnusedAllowance - postFlexiSavings).max(0)
      detail("allowance.ccf.calculation",
             s"aa:${currency(AA)};op: + ;" + detail("allowance.unused3y.calculation") + "op: - ;ats:" + detail("ats.calculation"))
      detail("allowance.ccf.calculation.reason","nmpa1")
      v
    } else {
      val v = (unusedAllowance.min(MAX_CF) + _previous3YearsUnusedAllowance).max(0)
      detail("allowance.ccf.calculation",s"cyunused:${currency(unusedAllowance.min(MAX_CF))};op: + ;" + detail("ats.calculation"))
      detail("allowance.ccf.calculation.reason","nmpa2")
      v
    }
  override def annualAllowanceCCF(): Long = { Logger.debug(s"annualAllowanceCCF() = ${currency(_aaCCF)}"); _aaCCF }

  protected lazy val _aaaCCF =
    isMPAAApplicable match {
      case true if _isACA => {
        val aaa = (unusedAAA + _previous3YearsUnusedAllowance)
        val v = (aaa).max(0).min(_aaCCF)
        detail("allowance.alt.ccf.calculation",s"cyunused:${currency(v)}")
        v
      }
      case true => {
        val v = AAA - preTriggerSavings
        val aaa = ((v).min(P2AAA) + _previous3YearsUnusedAllowance)
        if (v < P2AAA) {
          detail("allowance.alt.ccf.calculation",
                 s"aaa:${currency(AAA)};op: - ;pts:${currency(preTriggerSavings)};op: + ;" + detail("allowance.unused3y.calculation"))
        } else {
          detail("allowance.alt.ccf.calculation",s"aaa:${currency(P2AAA)};op: + ;" + detail("allowance.unused3y.calculation"))
        }
        detail("allowance.alt.ccf.calculation.reason","aca")
        (aaa).max(0)
      }
      case false => {
        detail("allowance.alt.ccf.calculation",s"aaa:0;")
        detail("allowance.alt.ccf.calculation.reason","nmpa")
        0L
      }
    }
  override def availableAAAWithCCF(): Long = _aaaCCF // same value as aaCCF considers AAA in calculation

  // Annual Allowance With Carry Forwards
  protected lazy val _aaCF = if (!isTriggered) {
                                detail("allowance.cf.calculation",s"aa:${currency(annualAllowance)};op:+;aaccf:${currency(previous.availableAAWithCCF)};")
                                detail("allowance.cf.calculation.reason","nte")
                                annualAllowance + previous.availableAAWithCCF
                             } else {
                                detail("allowance.cf.calculation",s"aa:${currency(previous.availableAllowance)};op:+;aaccf:${currency(year2014CCF)};")
                                detail("allowance.cf.calculation.reason","te")
                                previous.availableAAWithCF
                             }
  override def annualAllowanceCF(): Long = _aaCF

  override def availableAAAWithCF(): Long = if (!isTriggered) {
         0L
       } else {
         val v = alternativeAA() + _previous3YearsUnusedAllowance
         detail("allowance.alt.cf.calculation",s"aaa:${currency(alternativeAA())};op: + ;aaccf:${currency(_previous3YearsUnusedAllowance)}")
         detail("allowance.alt.cf.calculation.reason","te")
         v
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
  protected lazy val _alternativeChargableAmount =
    if (isMPAAApplicable) {
      detail("aca.calculation",s"mpist:${currency(mpist)};op: + ;dbist:${currency(dbist)};")
      mpist + dbist
    } else {
      detail("aca.calculation","mpist:0;")
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
                                          val v = basicCalculator().chargableAmount
                                          detail("chargable.calculation",s"db:${currency(definedBenefit)};op:-;unusedcf:${currency(annualAllowanceCF)};")
                                          v
                                        } else if (isMPAAApplicable) {
                                          val v = alternativeChargableAmount.max(defaultChargableAmount)
                                          if (alternativeChargableAmount > defaultChargableAmount) {
                                            detail("chargable.calculation",detail("aca.calculation"))
                                          } else {
                                            detail("chargable.calculation",detail("dca.calculation"))
                                          }
                                          v
                                        } else {
                                          val v = defaultChargableAmount
                                          detail("chargable.calculation",detail("dca.calculation"))
                                          v
                                        }
  override def chargableAmount(): Long = _chargableAmount

  // Cumulative Defined Benefit
  protected lazy val _cumulativeDB = definedBenefit
  override def cumulativeDB(): Long = _cumulativeDB

  // Cumulative Money Purchase
  protected lazy val _cumulativeMP = definedContribution
  override def cumulativeMP(): Long = _cumulativeMP

  // DBIST
  val year2014CCF = previousPeriods.filter(isBefore2015).headOption.map(_.summaryResult).getOrElse(SummaryResult()).availableAAWithCCF
  protected lazy val _dbist = {
    if (isTriggered) {
      val unusedaaa = preTriggerFields(previousPeriods).map(_.unusedAAA).getOrElse(0L)
      val allowances = unusedaaa + year2014CCF
      if (definedBenefit < allowances) {
        detail("dbist.calculation",s"dbist:0;")
        0L
      } else {
        val v = (allowances - definedBenefit).max(0)
        detail("dbist.calculation",s"unusedaaacf:${currency(unusedaaa)};op: + ;unused_2014:${currency(year2014CCF)};op: - ;pfs:${currency(definedBenefit)};")
        v
      }
    } else {
      val v = (preTriggerSavings - year2014CCF).max(0)
      detail("dbist.calculation",detail("pfs.calculation") + s"op:-;unused_2014:${currency(year2014CCF)};")
      v
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
      detail("dca.calculation","dca:0;")
      0L
    } else {
      val v = (postFlexiSavings - (AA + _previous3YearsUnusedAllowance)).max(0L)
      detail("dca.calculation",
             s"sep:(;cyps:${currency(postFlexiSavings)};" +
             s"op:-;sep:(;aa:${currency(AA)};sep:);op:+;sep:(;aaccf:${currency(_previous3YearsUnusedAllowance)};sep:);")
      v
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
  override def moneyPurchaseAA(): Long = {
    detail("allowance.mpa.cf.calculation",s"mpa:${currency(MPA)}")
    MPA
  }

  // MPIST
  protected lazy val _mpist = if (isMPAAApplicable) {
                                val v = definedContribution - MPA
                                detail("mpist.calculation",s"afs:${currency(definedContribution)};op: - ;mpa:${currency(MPA)};")
                                v
                              } else {
                                val v = definedContribution
                                detail("mpist.calculation",s"mpist:${currency(definedContribution)};")
                                v
                              }
  override def mpist(): Long = _mpist

  // Pre-Flexi Access Savings
  protected lazy val _preFlexiSavings = if (isTriggered) {
                                          preTriggerSavings
                                        } else {
                                          definedContribution + definedBenefit
                                        }
  override def preFlexiSavings() : Long = _preFlexiSavings

  protected lazy val _preTriggerSavings = preTriggerInputs(previousPeriods).map{(c)=>
      detail("pts.calculation",s"db:${currency(c.definedBenefit)};op:+;mp:${currency(c.moneyPurchase)};")
      c.definedBenefit + c.moneyPurchase
    }.getOrElse{
      detail("pts.calculation","0")
      0L
    }
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
                                    val v = (AAA - preTriggerSavings).min(P2AAA).max(0)
                                    Logger.debug(s"unusedAAA(mpa): ${currency(AAA)} - ${currency(definedBenefit)}")
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
}
// scalastyle:on number.of.methods

protected case class Period1Calculator(implicit allowanceInPounds: Long,
                                                previousPeriods:Seq[TaxYearResults],
                                                contribution: Contribution) extends Year2015Period1Calculator {
}

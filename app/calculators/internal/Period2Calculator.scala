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

protected trait Year2015Period2Calculator extends PeriodCalculator with DetailsCalculator {
  base: PeriodCalculator =>
  Logger.debug(s"\n***************************** 2015 Period 2 ${contribution.amounts} *****************************")

  def allowanceInPounds(): Long
  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  val MPA = 10000 * 100L
  val P1MPA = 20000 * 100L
  val P2MPA = 10000 * 100L
  val AAA = 30000 * 100L
  val MAXAACF = 40000 * 100L

  def allowance(): Long = allowanceInPounds

  // Annual Allowance Cumulative Carry Forwards
  protected lazy val _aaCF = if (isTriggered && isGroup3) {
                               period1.availableAAWithCCF
                             } else {
                               previous.availableAAWithCCF
                             }
  override def annualAllowanceCF(): Long = _aaCF

  override def availableAAAWithCF(): Long = if (!isTriggered) {
         0L
       } else {
         //val v = alternativeAA() + _previous3YearsUnusedAllowance
         //Logger.debug(s"AAACF: ${alternativeAA()} + ${_previous3YearsUnusedAllowance} = ${v}")
         val v = _aaCF
         Logger.debug(s"AAACF: ${v}")
         v
       }

  // Annual Allowance With Carry Forwards
  protected lazy val _unusedccf = {
    val list = _previous3YearsUnusedAllowanceList

    val pre = if (isPeriod1Triggered) definedBenefit else preFlexiSavings
    val sub = if (isPeriod1Triggered) period1.unusedAllowance else if (_isNonMemberInPeriod1) (if (_isACA) AAA else MAXAACF) else period1.unusedAllowance
    val exceeding = pre - sub
    val year2012AA = if (list.length >= 3) list(2)._2 else 0
    val year2013AA = if (list.length >= 2) list(1)._2 else 0
    val year2014AA = if (list.length >= 1) list(0)._2 else 0
    Logger.debug(s"_unusedccf (year2012AA): ${year2012AA}")
    Logger.debug(s"_unusedccf (year2013AA): ${year2013AA}")
    Logger.debug(s"_unusedccf (year2014AA): ${year2014AA}")
    Logger.debug(s"_unusedccf (exceeding): ${preFlexiSavings} - ${AAA} = ${exceeding}")
    if (exceeding > year2012AA) {
      val exceeding2 = exceeding - year2012AA
      if (exceeding2 > year2013AA) {
        val exceeding3 = exceeding2 - year2013AA
        if (exceeding3 > year2014AA) {
          val exceeding4 = exceeding3 - year2014AA
          if (exceeding4 > 0) {
            Logger.debug(s"_unusedccf (1): ${0}")
            0
          } else {
            Logger.debug(s"_unusedccf (2): ${exceeding4}")
            (exceeding4)
          }
        } else {
          Logger.debug(s"_unusedccf (3): ${year2014AA} - ${exceeding3} = ${year2014AA-exceeding3}")
          (year2014AA - exceeding3)
        }
      } else {
        Logger.debug(s"_unusedccf (4): ${year2013AA} - ${exceeding2} + ${year2014AA} = ${year2013AA-exceeding2+year2014AA}")
        (year2013AA - exceeding2) + year2014AA
      }
    } else {
      Logger.debug(s"_unusedccf (5): ${year2013AA} + ${year2014AA} = ${year2013AA+year2014AA}")
      (year2013AA + year2014AA)
    }
  }

  protected lazy val _aaCCF =
    if (!isTriggered) {
      val v = (period1.availableAAWithCCF - basicDefinedBenefit).max(0)
      Logger.debug(s"AACCF (nte): ${v}")
      v
    } else {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3) {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA - definedBenefit).max(0L)
          Logger.debug(s"AACCF(g3): ${previous2YearsUnusedAllowance} + ${previous.unusedAAA} + ${definedBenefit} = ${v}")
          v
        } else {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA).max(0L)
          Logger.debug(s"AACCF(g2): ${previous2YearsUnusedAllowance} + ${previous.unusedAAA} = ${v}")
          v
        }
      } else {
        if (unusedAllowance > 0) {
          val v = previous2YearsUnusedAllowance + unusedAllowance
          Logger.debug(s"AACCF(ua): ${previous2YearsUnusedAllowance} + ${unusedAllowance} = ${v}")
          v
        } else {
          if (_isACA) {
            val v = (_unusedccf).max(0L)
            Logger.debug(s"AACCF(aca): ${_unusedccf} = ${v}")
            v
          } else {
            val exceedingAAAmount = preTriggerFields(previousPeriods).map(_.exceedingAAAmount).getOrElse(0L)
            Logger.debug(s"!!!!!!!!!!!!!!! ${exceedingAAAmount}")
            if (exceedingAAAmount > 0) {
              if (_isACA) {
                val isNotRegisteredInP1 = previousPeriods.find(_.input.isPeriod1).map((r)=>r.input.moneyPurchase == 0 && r.input.definedBenefit == 0).getOrElse(false)
                if (isNotRegisteredInP1) {
                  Logger.debug(s"AACCF(>1): ${_unusedccf}")
                  (_unusedccf).max(0L)
                } else {
                  val v = (_previous3YearsUnusedAllowance - exceedingAAAmount).max(0L)
                  Logger.debug(s"AACCF(>2): ${_previous3YearsUnusedAllowance} - ${exceedingAAAmount} = ${v}")
                  v
                }
              } else {
                val v = (_previous3YearsUnusedAllowance - exceedingAAAmount - contribution.moneyPurchase).max(0L)
                Logger.debug(s"AACCF(>3): ${_previous3YearsUnusedAllowance} - ${exceedingAAAmount} - ${contribution.moneyPurchase} = ${v}")
                v
              }
            } else {
              Logger.debug(s"AACCF(<): ${_unusedccf}")
              (_unusedccf).max(0L)
            }
          }
        }
      }
    }
  override def annualAllowanceCCF(): Long = _aaCCF

  protected lazy val _aaaCCF =
    if (!isTriggered) {
      val v = 0L
      Logger.debug(s"AACCF (nte): ${v}")
      v
    } else {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3) {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA - definedBenefit).max(0L)
          Logger.debug(s"AACCF(g3): ${previous2YearsUnusedAllowance} + ${previous.unusedAAA} + ${definedBenefit} = ${v}")
          v
        } else {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA).max(0L)
          Logger.debug(s"AACCF(g2): ${previous2YearsUnusedAllowance} + ${previous.unusedAAA} = ${v}")
          v
        }
      } else {
        if (unusedAllowance > 0) {
          val v = previous2YearsUnusedAllowance + unusedAllowance
          Logger.debug(s"AACCF(ua): ${previous2YearsUnusedAllowance} + ${unusedAllowance} = ${v}")
          v
        } else {
          if (_isACA) {
            val v = (_unusedccf).max(0L)
            Logger.debug(s"AACCF(aca): ${_unusedccf} = ${v}")
            v
          } else {
            val exceedingAAAmount = preTriggerFields(previousPeriods).map(_.exceedingAAAmount).getOrElse(0L)
            Logger.debug(s"!!!!!!!!!!!!!!! ${exceedingAAAmount}")
            if (exceedingAAAmount > 0) {
              if (_isACA) {
                val isNotRegisteredInP1 = previousPeriods.find(_.input.isPeriod1).map((r)=>r.input.moneyPurchase == 0 && r.input.definedBenefit == 0).getOrElse(false)
                if (isNotRegisteredInP1) {
                  Logger.debug(s"AACCF(>1): ${_unusedccf}")
                  (_unusedccf).max(0L)
                } else {
                  val v = (_previous3YearsUnusedAllowance - exceedingAAAmount).max(0L)
                  Logger.debug(s"AACCF(>2): ${_previous3YearsUnusedAllowance} - ${exceedingAAAmount} = ${v}")
                  v
                }
              } else {
                val v = (_previous3YearsUnusedAllowance - exceedingAAAmount - contribution.moneyPurchase).max(0L)
                Logger.debug(s"AACCF(>3): ${_previous3YearsUnusedAllowance} - ${exceedingAAAmount} - ${contribution.moneyPurchase} = ${v}")
                v
              }
            } else {
              Logger.debug(s"AACCF(<): ${_unusedccf}")
              (_unusedccf).max(0L)
            }
          }
        }
      }
    }
  override def availableAAAWithCCF(): Long = _aaaCCF

  // Alternative Annual Allowance
  protected lazy val _alternativeAA = if (isGroup3 || (isGroup2 && isTriggered)) {
                                        if (isPeriod1Triggered && period1.isMPA) {
                                          previous.unusedAAA
                                        }
                                        else
                                          AAA
                                      } else {
                                        0L
                                      }
  override def alternativeAA(): Long = _alternativeAA

  // Alternative Chargable Amount
  protected lazy val _alternativeChargableAmount =
    if (isGroup3 && (isMPAAApplicable || period1.isMPA)) {
      (mpist + dbist).max(0)
    } else if (isGroup2) {
      if (isMPAAApplicable) {
        if (isPeriod1Triggered) {
          (definedContribution - previous.unusedMPAA).max(0)
        } else {
          (definedContribution - P2MPA).max(0)
        }
      } else {
        if (previous.unusedMPAA < definedContribution) {
          (definedContribution - previous.unusedMPAA).max(0)
        } else {
          0L
        }
      }
    } else {
      0L
    }
  override def alternativeChargableAmount(): Long = _alternativeChargableAmount

  // Annual Allowance
  protected lazy val _annualAllowance = period1.unusedAllowance
  override def annualAllowance(): Long = _annualAllowance

  def basicCalculator(): SummaryCalculator = BasicAllowanceCalculator(allowance, previousPeriods, contribution)

  // Chargable Amount (tax due)
  protected lazy val _chargableAmount =
    if (isTriggered) {
      if (isMPAAApplicable) {
        alternativeChargableAmount.max(defaultChargableAmount) // if aca == dca then choose dca
      } else {
        defaultChargableAmount
      }
    } else {
      (basicDefinedBenefit - previous.availableAAWithCCF).max(0L)
    }
  override def chargableAmount(): Long = _chargableAmount

  // Cumulative Defined Benefit
  protected lazy val _cumulativeDB = definedBenefit + previous.cumulativeDB
  override def cumulativeDB(): Long = _cumulativeDB

  // Cumulative Money Purchase
  protected lazy val _cumulativeMP = definedContribution + previous.cumulativeMP
  override def cumulativeMP(): Long = _cumulativeMP

  // DBIST
  protected lazy val _dbist =
    if (isGroup3) {
      if (isPeriod1Triggered) {
        val v = (definedBenefit - (preTriggerFields(previousPeriods).get.unusedAAA + period1.availableAAWithCCF)).max(0)
        Logger.debug(s"DBIST(p1te): ${definedBenefit} - (${preTriggerFields(previousPeriods).get.unusedAAA} + ${period1.availableAAWithCCF}) = ${v}")
        v
      } else {
        val v = (preFlexiSavings - period1.availableAAWithCCF).max(0)
        Logger.debug(s"DBIST(te): ${preFlexiSavings} - ${period1.availableAAWithCCF} = ${v}")
        v
      }
    } else {
      Logger.debug(s"DBIST(nte): 0")
      0L
    }
  override def dbist(): Long = _dbist

  // Default Chargable Amount
  protected lazy val _defaultChargableAmount =
    if (isGroup3 && isTriggered) {
      if (isPeriod1Triggered) {
        if (period1.isMPA) {
          /*
            // TODO 2015 Period 2: scenario 26 only
            val savings = previous.preFlexiSavings + previous.postFlexiSavings
            val aacf = previous.availableAAWithCF
            postFlexiSavings - previous.dcaCF
          */
          if (period1.isACA) {
            ((definedContribution + definedBenefit) - (period1.availableAAAWithCCF)).max(0)
          } else {
            ((definedContribution + definedBenefit) - (_previous3YearsUnusedAllowance + period1.unusedAAA)).max(0)
          }
        } else {
          ((definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
        }
      } else {
        ((preFlexiSavings + definedContribution) - period1.availableAAWithCCF).max(0)
      }
    } else if (isGroup2 && isTriggered) {
      if (previous.unusedAAA > 0) {
        (mpist - (previous.unusedAAA + previous.availableAAWithCCF)).max(0)
      } else if (isPeriod1Triggered) {
        (mpist - (previous.unusedAllowance + previous.availableAAWithCCF)).max(0)
      } else {
        ((mpist + previous.mpist) - (period1.unusedAllowance + previous.availableAAWithCCF)).max(0)
      }
    } else {
      0L
    }
  override def defaultChargableAmount(): Long = _defaultChargableAmount

  // Defined Benefit
  def basicDefinedBenefit(): Long = basicCalculator().definedBenefit

  protected lazy val _definedBenefit =
    if (isGroup3) {
      if (isPeriod2Triggered) {
        previous.cumulativeDB
      } else {
        basicDefinedBenefit
      }
    } else if (isGroup2) {
      0L // definition of group 2 is that there is no db
    }
    else {
      basicDefinedBenefit
    }
  override def definedBenefit(): Long = _definedBenefit

  protected lazy val _definedContribution = basicCalculator.definedContribution
  def definedContribution(): Long = _definedContribution

  // Exceeding Alternative Annual Allowance
  override def exceedingAAA(): Long = 0L

  // Exceeding Annual Allowance
  protected lazy val _exceedingAllowance =
    if ((isGroup2 || isGroup3) && isTriggered) {
      0L
    } else {
      (basicDefinedBenefit - period1.unusedAllowance).max(0)
    }
  override def exceedingAllowance(): Long = _exceedingAllowance

  // Exceeding Money Purchase Allowance
  protected lazy val _exceedingMPAA =
    if (isMPAAApplicable) {
      (definedContribution - MPA).max(0)
    } else {
      0L
    }
  override def exceedingMPAA(): Long = _exceedingMPAA

  protected lazy val isGroup2: Boolean = !contribution.isGroup3 && contribution.isGroup2
  protected lazy val isGroup3: Boolean = contribution.isGroup3

  // Is ACA Applicable
  protected lazy val _isACA = isTriggered && alternativeChargableAmount > defaultChargableAmount
  override def isACA(): Boolean = _isACA

  // Is MPA Applicable
  protected lazy val _isPeriod1MPA: Boolean =
    period1.isMPA ||
    period1.cumulativeMP >= P1MPA ||
    (previous.unusedMPAA > 0 && previous.unusedMPAA < definedContribution)

  protected lazy val _isMPAAApplicable: Boolean =
    if (isTriggered) {
      definedContribution > MPA || _isPeriod1MPA
    } else {
      false
    }
  override def isMPAAApplicable(): Boolean = _isMPAAApplicable

  def isTriggered(): Boolean = contribution.isTriggered

  protected lazy val isPeriod1Triggered: Boolean = previousPeriods.find(isTaxResultTriggered).find(_.input.isPeriod1).isDefined

  protected lazy val isPeriod2Triggered: Boolean = isTriggered && !isPeriod1Triggered

  // Money Purchase Annual Allowance
  protected lazy val _moneyPurchaseAA = if (isPeriod1Triggered) period1.unusedMPAA else MPA
  override def moneyPurchaseAA(): Long = _moneyPurchaseAA

  // MPIST
  protected lazy val _mpist =
    if (isGroup3) {
      if (isPeriod1Triggered) {
        val v = (definedContribution - period1.unusedMPAA).max(0)
        Logger.debug(s"MPIST(p1te): ${definedContribution} - ${period1.unusedMPAA} = ${v}")
        v
      } else if (isMPAAApplicable) {
        val v = (definedContribution - MPA).max(0)
        Logger.debug(s"MPIST(p2te): ${definedContribution} - ${MPA} = ${v}")
        v
      } else {
        Logger.debug(s"MPIST(nte): 0")
        0L
      }
    } else {
      Logger.debug(s"MPIST(g2): ${definedContribution}")
      definedContribution
    }
  override def mpist(): Long = _mpist

  protected lazy val period1: ExtendedSummaryFields = previousPeriods.find(_.input.isPeriod1).flatMap(maybeExtended(_)).getOrElse(ExtendedSummaryFields())

  // Pre-Flexi Access Savings
  protected lazy val _isNonMemberInPeriod1 =
    if (isPeriod2Triggered) {
      preTriggerInputs(previousPeriods).map((c)=>c.moneyPurchase == 0 && c.definedBenefit == 0).getOrElse(false)
    } else {
      false
    }
  protected lazy val _preFlexiSavings =
    if (isPeriod2Triggered) {
      preTriggerInputs(previousPeriods).map((c)=>c.moneyPurchase + c.definedBenefit).getOrElse(0L)
    } else {
      0L
    }
  override def preFlexiSavings() : Long = _preFlexiSavings

  protected lazy val previous: ExtendedSummaryFields = previousPeriods.headOption.flatMap(maybeExtended(_)).getOrElse(ExtendedSummaryFields())

  protected lazy val previous2YearsUnusedAllowance: Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(2015, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_.input.isPeriod2)
    actualUnusedList(this)(pp, c).dropWhile(_._1 == 2015).slice(0,2).foldLeft(0L)(_ + _._2)
  }

  protected lazy val _previous3YearsUnusedAllowance = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(2015, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_.input.isPeriod2)
    Logger.debug(s"""Pre-3Years Unused: ${actualUnusedList(this)(pp, c).dropWhile(_._1 == 2015).slice(0,3).mkString("\n")}""")
    actualUnusedList(this)(pp, c).dropWhile(_._1 == 2015).slice(0,3).foldLeft(0L)(_ + _._2)
  }

  protected lazy val _previous3YearsUnusedAllowanceList = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(2015, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_.input.isPeriod2)
    Logger.debug(s"""Pre-3Years Unused: ${actualUnusedList(this)(pp, c).dropWhile(_._1 == 2015).slice(0,3).mkString("\n")}""")
    actualUnusedList(this)(pp, c).dropWhile(_._1 == 2015)
  }

  // Post Flexi Access Savings
  protected lazy val _postFlexiSavings =
    if (isTriggered) {
      definedContribution + definedBenefit
    } else {
      0L
    }
  override def postFlexiSavings() : Long = _postFlexiSavings

  // Unused Alternative Annual Allowance
  protected lazy val _unusedAAA =
    if (isTriggered) {
      if (isGroup3) {
        val v = (period1.unusedAAA - contribution.definedBenefit).max(0)
        Logger.debug(s"unusedAAA(g3): ${period1.unusedAAA} - ${contribution.definedBenefit} = ${v}")
        v
      } else {
        val v = previous.unusedAAA.max(0)
        Logger.debug(s"unusedAAA(g2): ${v}")
        v
      }
    } else {
      0L
    }
  override def unusedAAA(): Long = _unusedAAA

  // Unused Annual Allowance
  protected lazy val _group3Unused =
    if (period1.isMPA) {
      period1.unusedAAA - definedBenefit
    } else if ((preFlexiSavings + (if (defaultChargableAmount >= alternativeChargableAmount) definedContribution else 0L)) > MAXAACF) {
      0L
    } else if (isPeriod1Triggered) {
      if (alternativeChargableAmount > defaultChargableAmount) {
        (period1.unusedAllowance - contribution.definedBenefit).max(0L)
      } else {
        (period1.unusedAllowance - (contribution.definedBenefit + contribution.moneyPurchase)).max(0L)
      }
    } else {
      previousPeriods.headOption.map(_.input).map {
        (previousSavings) =>
        if (alternativeChargableAmount > defaultChargableAmount) {
          (period1.unusedAllowance - (previousSavings.definedBenefit + previousSavings.moneyPurchase) ).max(0L)
        } else {
          (period1.unusedAllowance - (previousSavings.definedBenefit + previousSavings.moneyPurchase + contribution.moneyPurchase)).max(0L)
        }
      }.getOrElse(0L)
    }

  protected lazy val _unusedAllowance =
    if (isTriggered) {
      if (previous.unusedAAA > 0) {
        0L
      } else if (isGroup3) {
        _group3Unused.max(0)
      } else if (period1.cumulativeMP < P1MPA && definedContribution < MPA) {
        (previous.unusedAllowance - definedContribution).max(0)
      } else if (alternativeChargableAmount > defaultChargableAmount) {
        previous.unusedAllowance
      } else {
        (previous.unusedAllowance - definedContribution).max(0)
      }
    } else {
      (period1.unusedAllowance - basicDefinedBenefit).max(0)
    }
  override def unusedAllowance(): Long = _unusedAllowance

  // Unused Money Purchase Annual Allowance
  override def unusedMPAA(): Long = 0
  Logger.debug(s"\n***************************** 2015 Period 2 (end) *****************************")
}

protected case class Period2Calculator(implicit allowanceInPounds: Long,
                                                previousPeriods:Seq[TaxYearResults],
                                                contribution:Contribution) extends Year2015Period2Calculator {
}

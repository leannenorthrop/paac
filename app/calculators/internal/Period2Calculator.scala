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
                                detail("allowance.cf.calculation",s"aaccf:${currency(period1.availableAAWithCCF)}")
                                detail("allowance.cf.calculation.reason","g3")
                                period1.availableAAWithCCF
                             } else {
                                detail("allowance.cf.calculation",s"aaccf:${currency(previous.availableAAWithCCF)}")
                                detail("allowance.cf.calculation.reason","ng3")
                                previous.availableAAWithCCF
                             }
  override def annualAllowanceCF(): Long = _aaCF

  override def availableAAAWithCF(): Long = if (!isTriggered) {
         0L
       } else {
         val v = _aaCF
         detail("allowance.alt.cf.calculation",s"aaa:${currency(v)}")
         detail("allowance.alt.cf.calculation.reason","te")
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
    if (exceeding > year2012AA) {
      val exceeding2 = exceeding - year2012AA
      if (exceeding2 > year2013AA) {
        val exceeding3 = exceeding2 - year2013AA
        if (exceeding3 > year2014AA) {
          val exceeding4 = exceeding3 - year2014AA
          if (exceeding4 > 0) {
            detail("_unusedccf.calculation",s"unused_2015:0")
            0
          } else {
            detail("_unusedccf.calculation",s"unused_2015:${currency(exceeding4)}")
            (exceeding4)
          }
        } else {
          val v = (year2014AA - exceeding3)
          detail("_unusedccf.calculation",s"unused_2015:${currency(v)}")
          v
        }
      } else {
        val v = (year2013AA - exceeding2) + year2014AA
        detail("_unusedccf.calculation",s"unused_2015:${currency(v)}")
        v
      }
    } else {
      val v = (year2013AA + year2014AA)
      detail("_unusedccf.calculation",s"unused_2015:${currency(v)}")
      v
    }
  }

  def ccfdetails: Unit = {
    val desc = _previous3YearsUnusedAllowanceList.slice(0,2) match {
      case head :: Nil => s"unused_${head._1}:${fmt(head._2)};"
      case head :: tail => tail.foldLeft(s"unused_${head._1}:${fmt(head._2)};")((str,pair)=>str + s"op:+;unused_${pair._1}:${fmt(pair._2)};")
      case _ => ""
    }
    detail("allowance.ccf.calculation.details",desc)
  }

  protected lazy val _aaCCF =
    if (!isTriggered) {
      val year2012AA = if (_previous3YearsUnusedAllowanceList.length >= 3) _previous3YearsUnusedAllowanceList(2)._2 else 0
      val v = (period1.availableAAWithCCF - basicDefinedBenefit - year2012AA).max(0)
      ccfdetails
      //detail("allowance.ccf.calculation",s"aaccf:${currency(period1.availableAAWithCCF)};op:-;db:${currency(basicDefinedBenefit)};op:-;unused_2015:${currency(year2012AA)};")
      detail("allowance.ccf.calculation",s"unused_2015:${currency(unusedAllowance)};op:+;"+detail("allowance.ccf.calculation.details"))
      detail("allowance.ccf.calculation.reason","nte")
      v
    } else {
      if (previous.unusedAAA > 0) {
        if (contribution.isGroup3) {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA - definedBenefit).max(0L)
          detail("allowance.ccf.calculation",s"aaccf:${currency(previous2YearsUnusedAllowance)};op:+;unusedaaacf:${currency(previous.unusedAAA)};op:-;db:${currency(definedBenefit)};")
          detail("allowance.ccf.calculation.reason","g3")
          v
        } else {
          val v = (previous2YearsUnusedAllowance + previous.unusedAAA).max(0L)
          detail("allowance.ccf.calculation",s"aaccf:${currency(previous2YearsUnusedAllowance)};op:+;unusedaaacf:${currency(previous.unusedAAA)};")
          detail("allowance.ccf.calculation.reason","g2")
          v
        }
      } else {
        if (unusedAllowance > 0) {
          val v = previous2YearsUnusedAllowance + unusedAllowance
          detail("allowance.ccf.calculation",s"aaccf:${currency(previous2YearsUnusedAllowance)};op:+;unusedaaacf:${currency(unusedAllowance)};")
          detail("allowance.ccf.calculation.reason","g1")
          v
        } else {
          if (_isACA) {
            val v = (_unusedccf).max(0L)
            detail("allowance.ccf.calculation",s"aaccf:${currency(_unusedccf)};")
            detail("allowance.ccf.calculation.reason","aca")
            v
          } else {
            val exceedingAAAmount = preTriggerFields(previousPeriods).map(_.exceedingAAAmount).getOrElse(0L)
            if (exceedingAAAmount > 0) {
              if (_isACA) {
                val isNotRegisteredInP1 = previousPeriods.find(_.input.isPeriod1).map((r)=>r.input.moneyPurchase == 0 && r.input.definedBenefit == 0).getOrElse(false)
                if (isNotRegisteredInP1) {
                  detail("allowance.ccf.calculation",s"aaccf:${currency(_unusedccf)};")
                  detail("allowance.ccf.calculation.reason","aca1")
                  (_unusedccf).max(0L)
                } else {
                  val v = (_previous3YearsUnusedAllowance - exceedingAAAmount).max(0L)
                  detail("allowance.ccf.calculation",s"aaccf:${currency(_previous3YearsUnusedAllowance)};op:-;unusedcf:${currency(exceedingAAAmount)};")
                  detail("allowance.ccf.calculation.reason","aca2")
                  v
                }
              } else {
                val v = (_previous3YearsUnusedAllowance - exceedingAAAmount - contribution.moneyPurchase).max(0L)
                detail("allowance.ccf.calculation",s"aaccf:${currency(_previous3YearsUnusedAllowance)};op:-;unusedcf:${currency(exceedingAAAmount)};op:-;mp:${currency(contribution.moneyPurchase)};")
                detail("allowance.ccf.calculation.reason","aca3")
                v
              }
            } else {
              val v = (_unusedccf).max(0L)
              detail("allowance.ccf.calculation",s"aaccf:${currency(v)};")
              detail("allowance.ccf.calculation.reason","aca4")
              v
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
                                          detail("unusedaaacf.calculation",s"unusedaaacf:${currency(previous.unusedAAA)};")
                                          previous.unusedAAA
                                        }
                                        else {
                                          detail("unusedaaacf.calculation",s"unusedaaacf:${currency(AAA)};")
                                          AAA
                                        }
                                      } else {
                                        detail("unusedaaacf.calculation",s"unusedaaacf:0;")
                                        0L
                                      }
  override def alternativeAA(): Long = _alternativeAA

  // Alternative Chargable Amount
  protected lazy val _alternativeChargableAmount =
    if (isGroup3 && (isMPAAApplicable || period1.isMPA)) {
      val v = (mpist + dbist).max(0)
      detail("aca.calculation",detail("mpist.calculation")+"op:+;"+detail("dbist.calculation"))
      v
    } else if (isGroup2) {
      if (isMPAAApplicable) {
        if (isPeriod1Triggered) {
          val v = (definedContribution - previous.unusedMPAA).max(0)
          detail("aca.calculation",s"mp:${currency(definedContribution)};op:-;mpa:${currency(previous.unusedMPAA)}")
          v
        } else {
          val v = (definedContribution - P2MPA).max(0)
          detail("aca.calculation",s"mp:${currency(definedContribution)};op:-;mpa:${currency(P2MPA)}")
          v
        }
      } else {
        if (previous.unusedMPAA < definedContribution) {
          val v = (definedContribution - previous.unusedMPAA).max(0)
          detail("aca.calculation",s"mp:${currency(definedContribution)};op:-;mpa:${currency(previous.unusedMPAA)}")
          v
        } else {
          detail("aca.calculation",s"aca:0")
          0L
        }
      }
    } else {
      detail("aca.calculation",s"aca:0")
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
        // if aca == dca then choose dca
        val v = alternativeChargableAmount.max(defaultChargableAmount)
        if (alternativeChargableAmount > defaultChargableAmount) {
          detail("chargable.calculation",detail("aca.calculation"))
        } else {
          detail("chargable.calculation",detail("dca.calculation"))
        }
        v
      } else {
        detail("chargable.calculation",detail("dca.calculation"))
        defaultChargableAmount
      }
    } else {
      val v = (basicDefinedBenefit - previous.availableAAWithCCF).max(0L)
      detail("chargable.calculation",s"db:${currency(basicDefinedBenefit)};op:-;aaccf:${currency(previous.availableAAWithCCF)}")
      v
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
        detail("dbist.calculation",s"db:${currency(definedBenefit)};op:-;unusedaaacf:${currency(preTriggerFields(previousPeriods).get.unusedAAA)};op:+;unused_2015:${currency(period1.availableAAWithCCF)};")
        v
      } else {
        val v = (preFlexiSavings - period1.availableAAWithCCF).max(0)
        detail("dbist.calculation",detail("pfs.calculation")+s"op:-;unused_2015:${currency(period1.availableAAWithCCF)};")
        v
      }
    } else {
      detail("dbist.calculation",s"dbist:0;")
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
            val v = ((definedContribution + definedBenefit) - (period1.availableAAAWithCCF)).max(0)
            detail("dca.calculation",s"mp:${currency(definedContribution)};op:+;db:${currency(definedBenefit)};op:-;aaaccf:${currency(period1.availableAAAWithCCF)};")
            v
          } else {
            val v = ((definedContribution + definedBenefit) - (_previous3YearsUnusedAllowance + period1.unusedAAA)).max(0)
            detail("dca.calculation",s"mp:${currency(definedContribution)};op:+;db:${currency(definedBenefit)};op:-;aaaccf:${currency(_previous3YearsUnusedAllowance)};op:+;unusedcf:${currency(period1.unusedAAA)};")
            v
          }
        } else {
          val v = ((definedContribution + definedBenefit) - period1.availableAAWithCCF).max(0)
          detail("dca.calculation",s"mp:${currency(definedContribution)};op:+;db:${currency(definedBenefit)};op:-;aaaccf:${currency(period1.availableAAWithCCF)};")
          v
        }
      } else {
        val v = ((preFlexiSavings + definedContribution) - period1.availableAAWithCCF).max(0)
        detail("dca.calculation",s"pts:${currency(preFlexiSavings)};op:+;db:${currency(definedContribution)};op:-;aaaccf:${currency(period1.availableAAWithCCF)};")
        v
      }
    } else if (isGroup2 && isTriggered) {
      if (previous.unusedAAA > 0) {
        val v = (mpist - (previous.unusedAAA + previous.availableAAWithCCF)).max(0)
        detail("dca.calculation",s"mpist:${currency(mpist)};op:+;unusedaaacf:${currency(previous.unusedAAA)};op:+;aaaccf:${currency(previous.availableAAWithCCF)};")
        v
      } else if (isPeriod1Triggered) {
        val v = (mpist - (previous.unusedAllowance + previous.availableAAWithCCF)).max(0)
        detail("dca.calculation",s"mpist:${currency(mpist)};op:+;unusedcf:${currency(previous.unusedAllowance)};op:+;aaaccf:${currency(previous.availableAAWithCCF)};")
        v
      } else {
        val v = ((mpist + previous.mpist) - (period1.unusedAllowance + previous.availableAAWithCCF)).max(0)
        detail("dca.calculation",s"mpist:${currency(mpist+previous.mpist)};op:-;unusedcf:${currency(period1.unusedAllowance )};op:+;aaaccf:${currency(previous.availableAAWithCCF)};")
        v
      }
    } else {
      detail("dca.calculation",s"dca:0;")
      0L
    }
  override def defaultChargableAmount(): Long = _defaultChargableAmount

  // Defined Benefit
  def basicDefinedBenefit(): Long = basicCalculator().definedBenefit

  protected lazy val _definedBenefit =
    if (isGroup3) {
      if (isPeriod2Triggered) {
        detail("_definedBenefit.calculation",s"db:${currency(previous.cumulativeDB)};")
        previous.cumulativeDB
      } else {
        detail("_definedBenefit.calculation",s"db:${currency(basicDefinedBenefit)};")
        basicDefinedBenefit
      }
    } else if (isGroup2) {
      detail("_definedBenefit.calculation",s"db:0;")
      0L // definition of group 2 is that there is no db
    }
    else {
      detail("_definedBenefit.calculation",s"db:${currency(basicDefinedBenefit)};")
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
        detail("mpist.calculation",s"mp:${currency(definedContribution)};op:-;unusedmpa:${currency(period1.unusedMPAA)};")
        v
      } else if (isMPAAApplicable) {
        val v = (definedContribution - MPA).max(0)
        detail("mpist.calculation",s"mp:${currency(definedContribution)};op:-;unusedmpa:${currency(MPA)};")
        v
      } else {
        detail("mpist.calculation",s"mpist:0;")
        0L
      }
    } else {
      detail("mpist.calculation",s"mpist:${currency(definedContribution)};")
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
      val v = preTriggerInputs(previousPeriods).map{(c)=>
        detail("pts.calculation",s"mp:${currency(c.moneyPurchase)};op:+;db:${currency(c.definedBenefit)};")
        c.moneyPurchase + c.definedBenefit
      }.getOrElse(0L)
      v
    } else {
      detail("pts.calculation",s"pts:0;")
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
}

protected case class Period2Calculator(implicit allowanceInPounds: Long,
                                                previousPeriods:Seq[TaxYearResults],
                                                contribution:Contribution) extends Year2015Period2Calculator {
}

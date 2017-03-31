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

import calculators._
import calculators.internal.utilities._
import models._
import config.PaacConfiguration
import play.api.Logger

// scalastyle:off number.of.methods
trait TaperedAllowanceCalculator extends ExtendedSummaryCalculator {
  Logger.debug(s"\n***************************** ${contribution.taxYear} *****************************")
  private val DEFAULT_MPA = 10000
  private val DEFAULT_TAA = 10000
  private val DEAFULT_TAPER_START = 150000
  private val DEAFULT_TAPER_END = 210000
  private val DEAFULT_AA = 40000


  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  override def acaCF() : Long = _acaCF
  override def allowance(): Long = _annualAllowance
  override def alternativeAA(): Long = _alternativeAA
  override def alternativeChargableAmount(): Long = _alternativeChargableAmount
  override def annualAllowance(): Long = _taperedAllowance
  override def annualAllowanceCF(): Long = _annualAllowanceCF
  override def annualAllowanceCCF(): Long = _annualAllowanceCCF
  override def availableAAAWithCF(): Long = _alternativeAACF
  override def availableAAAWithCCF(): Long = _annualAllowanceCCF
  override def chargableAmount(): Long = _chargableAmount
  override def cumulativeDB(): Long = _cumulativeDB
  override def cumulativeMP(): Long = _cumulativeMP
  override def dbist(): Long = _dbist
  override def dcaCF() : Long = _dcaCF
  override def defaultChargableAmount(): Long = _defaultChargableAmount
  override def definedBenefit(): Long = _definedBenefit
  override def definedContribution(): Long = _definedContribution
  override def exceedingAAA(): Long = _exceedingAAA
  override def exceedingAllowance(): Long = _exceedingAllowance
  override def exceedingMPAA(): Long = _exceedingMPAA
  override def isACA(): Boolean = _isACA
  override def isMPAAApplicable(): Boolean = _isMPAAApplicable
  override def moneyPurchaseAA(): Long = _mpa
  override def mpist(): Long = _mpist
  override def postFlexiSavings(): Long = _postFlexiSavings
  override def preFlexiSavings(): Long = _preTriggerSavings
  override def unusedAAA(): Long = _unusedAAA
  override def unusedAllowance(): Long = _unusedAllowance
  override def unusedMPAA(): Long = _unusedMPAA

  protected lazy val income: Long =
    if (isTriggered) {
      previousPeriods.headOption.map {
        (previous) =>
        if (previous.input.isTriggered) {
          contribution.income
        } else {
          previous.input.income
        }
      }.getOrElse(contribution.income)
    }
    else {
      contribution.income
    }

  protected def isTaperingApplicable(): Boolean = income > _taperStart

  protected def isTriggered(): Boolean = contribution.isTriggered

  protected lazy val actualUnused = actualUnusedList(this)(previousPeriods,contribution)

  protected lazy val actualAAAUnused = actualAAAUnusedList(this)(previousPeriods,contribution)

  protected lazy val config: Map[String,Int] =
    PaacConfiguration.forYear(contribution.taxPeriodStart.taxYear)

  protected lazy val previousYear =
    previousPeriods.find(isYear(contribution.taxPeriodStart.taxYear-1))

  protected lazy val previous3YearsUnusedAllowance: Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val taxYear = contribution.taxPeriodStart.taxYear
    val c = Contribution(taxYear, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_._1 == taxYear)
    val calc = BasicAllowanceCalculator(0,pp,c)
    val unused = actualUnusedList(calc)(pp, c).dropWhile(_._1 == taxYear).slice(0,3)
    Logger.debug(s"""3 Years Unused: ${unused.mkString(", ")}""")
    unused.foldLeft(0L)(_ + _._2)
  }

  protected lazy val previous3YearsUnusedAAAllowance: Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val taxYear = contribution.taxPeriodStart.taxYear
    val c = Contribution(taxYear, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_._1 == taxYear)
    val calc = BasicAllowanceCalculator(0,pp,c)
    val unused = actualAAAUnused.dropWhile(_._1 == taxYear).slice(0,3)
    Logger.debug(s"""3 Years Unused AAA: ${unused.mkString(", ")}""")
    unused.foldLeft(0L)(_ + _._2)
  }

  protected lazy val previous2YearsUnusedAllowance: Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val taxYear = contribution.taxPeriodStart.taxYear
    val c = Contribution(taxYear, Some(InputAmounts(0L,0L)))
    val pp = previousPeriods.dropWhile(_._1 == taxYear)
    val calc = BasicAllowanceCalculator(0,pp,c)
    val unused = actualUnusedList(calc)(pp, c).dropWhile(_._1 == taxYear).slice(0,2)
    Logger.debug(s"""2 Years Unused: ${unused.mkString(", ")}""")
    unused.foldLeft(0L)(_ + _._2)
  }

  protected lazy val _acaCF = {
    val previousACACF = previousYear.flatMap(maybeExtended(_).map(_.acaCF)).getOrElse(0L)
    Logger.debug(s"""ACACF: ${alternativeChargableAmount} +  ${previousACACF}""")
    alternativeChargableAmount + previousACACF
  }

  protected lazy val _dcaCF = {
    val previousDCACF = previousYear.flatMap(maybeExtended(_).map(_.dcaCF)).getOrElse(0L)
    Logger.debug(s"""DCACF: ${defaultChargableAmount} +  ${previousDCACF}""")
    defaultChargableAmount + previousDCACF
  }

  protected lazy val _postFlexiSavings =
    if (isTriggered) {
      Logger.debug(s"PostFlexiSavings(te): ${contribution.moneyPurchase} ${contribution.definedBenefit}")
      contribution.moneyPurchase + contribution.definedBenefit
    }
    else {
      Logger.debug(s"PostFlexiSavings: 0")
      0L
    }

  protected lazy val _cumulativeMP =
    (for {
      prev <- previousYear
      prevResults <- maybeExtended(prev)
    } yield (definedContribution + prevResults.cumulativeMP)).getOrElse(definedContribution)

  protected lazy val _cumulativeDB =
    (for {
      prev <- previousYear
      prevResults <- maybeExtended(prev)
    } yield (definedBenefit + prevResults.cumulativeDB)).getOrElse(definedBenefit)

  protected lazy val _exceedingMPAA = (definedContribution - moneyPurchaseAA).max(0L)

  protected lazy val _exceedingAAA = definedBenefit - alternativeAA

  protected lazy val _isPreviousTriggered: Boolean =
    previousPeriods.headOption.map{
      (previous)=>
      previous.input.isTriggered
    }.getOrElse(false)


  protected lazy val _defaultChargableAmount =
    if (!isTriggered) {
      Logger.debug(s"DCA: 0")
      0L
    }
    else {
      if (_isPreviousTriggered) {
        val v = (postFlexiSavings - (annualAllowance + _previousAvailableAAWithCCF)).max(0L)
        Logger.debug(s"DCA(te < ${contribution.taxYear}): ${postFlexiSavings} - (${annualAllowance} + ${_previousAvailableAAWithCCF}) = $v")
        v
      } else {
        val v = ((postFlexiSavings + preFlexiSavings) - (annualAllowance + _previousAvailableAAWithCCF)).max(0L)
        Logger.debug(s"DCA(te == ${contribution.taxYear}): (${postFlexiSavings} + ${preFlexiSavings}) - (${annualAllowance} + ${_previousAvailableAAWithCCF}) = $v")
        v
      }
    }

  protected lazy val _annualAllowanceCCF =
    if (!isTriggered) {
      val v = actualUnused.slice(0,4).foldLeft(0L)(_ + _._2)
      Logger.debug(s"AACCF(nte): ${v}")
      v
    } else if (alternativeChargableAmount >= defaultChargableAmount) {
      val v = actualAAAUnused.slice(0,4).foldLeft(0L)(_ + _._2)
      Logger.debug(s"""3 Years Unused AAA: ${actualAAAUnused.slice(0,3).mkString(", ")}""")
      Logger.debug(s"AACCF(aca): ${v}")
      v
    }
    else {
      val v = if (_unusedAllowance > 0) _unusedAllowance else (_unusedAllowance - _exceedingAllowance).max(0L)
      Logger.debug(s"AACCF(dca): if ${_unusedAllowance > 0} ${_unusedAllowance} else ${_unusedAllowance} - ${_exceedingAllowance} = ${v}")
      v
    }

  protected lazy val _alternativeAACF = {
    contribution.taxPeriodStart.taxYear match {
      case year if year <= 2018 => {
        val v = _alternativeAA + previous3YearsUnusedAAAllowance
        Logger.debug(s"AAACF(<=2018): ${_alternativeAA} + ${previous3YearsUnusedAAAllowance}")
        v
      }
      case _ => {
        val v = _alternativeAA + previous3YearsUnusedAAAllowance
        Logger.debug(s"AAACF(>2018): ${_alternativeAA} + ${previous3YearsUnusedAAAllowance}")
        v
      }
    }
  }

  protected lazy val _annualAllowanceCF = annualAllowance() + previousYear.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  protected lazy val _unusedAllowance =
    if (isTriggered) {
      if (defaultChargableAmount >= alternativeChargableAmount) {
        Logger.debug(s"Unused AA (dca): ${annualAllowance} - (${preFlexiSavings} + ${definedContribution})")
        (annualAllowance - (preFlexiSavings + definedContribution)).max(0L)
      }
      else {
        Logger.debug(s"Unused AA (aca): ${unusedAAA}")
        unusedAAA
      }
    } else {
      val v = (annualAllowance - definedBenefit).max(0)
      Logger.debug(s"Unused AA(nte): ${annualAllowance} - ${definedBenefit}")
      v
    }

  protected lazy val _exceedingAllowance =
    if (alternativeChargableAmount >= defaultChargableAmount) {
      if (isTriggered) {
        val v = ((preFlexiSavings + definedContribution) - annualAllowance).max(0L)
        Logger.debug(s"Exceeding (aca te): ${preFlexiSavings} + ${definedContribution} - ${annualAllowance} = ${v}")
        v
      } else {
        val v = (preFlexiSavings - annualAllowance).max(0L)
        Logger.debug(s"Exceeding (aca nte): ${preFlexiSavings} - ${annualAllowance} = ${v}")
        v
      }
    }
    else {
      val v = 0L
      Logger.debug(s"Exceeding (dca): ${v}")
      v
    }

  // taper automatically applied as part of annualAllowance calculation
  protected lazy val _alternativeAA =
    if (isMPAAApplicable) {
      Logger.debug(s"AAA: ${annualAllowance()} - ${moneyPurchaseAA}")
      (annualAllowance() - moneyPurchaseAA).max(0L)
    } else {
      Logger.debug(s"AAA: 0")
      0L
    }

  protected lazy val _definedBenefit =
    if (isTriggered) {
      Logger.debug(s"DB(te): ${contribution.definedBenefit} + ${preFlexiSavings}")
      contribution.definedBenefit + preFlexiSavings
    }
    else {
      Logger.debug(s"DB: ${contribution.definedBenefit} + ${contribution.moneyPurchase}")
      contribution.definedBenefit + contribution.moneyPurchase
    }

  protected lazy val _definedContribution = contribution.moneyPurchase

  protected lazy val _chargableAmount = {
    if (!isTriggered) {
      val v = (_definedBenefit - annualAllowanceCF()).max(0)
      Logger.debug(s"Tax(!te): ${v}")
      v
    } else if (isMPAAApplicable) {
      Logger.debug(s"Tax(mpa): ${alternativeChargableAmount.max(defaultChargableAmount)}")
      alternativeChargableAmount.max(defaultChargableAmount)
    } else {
      Logger.debug(s"Tax(!mpa): ${defaultChargableAmount}")
      defaultChargableAmount
    }
  }

  protected lazy val _taperedAllowance =
    if (isTaperingApplicable) {
      income match {
        case i if i > _taperStart && i < _taperEnd => {
          val reduction = Math.floor(((i - _taperStart)/100L)/2D)*100L
          val v = (_annualAllowance - reduction).toLong
          Logger.debug(s"AA(tapered): ${v}")
          v
        }
        case i if i >= _taperEnd => {
          Logger.debug(s"AA(max tapered): ${_taa}")
          _taa
        }
        case _ => {
          Logger.debug(s"AA(no taper): ${_annualAllowance}")
          _annualAllowance
        }
      }
    } else {
      Logger.debug(s"AA(no taper): ${_annualAllowance}")
      _annualAllowance
    }

  protected lazy val isGroup1: Boolean = contribution.amounts.isDefined &&
                                         !contribution.amounts.get.moneyPurchase.isDefined &&
                                         !contribution.isTriggered

  protected lazy val isGroup2: Boolean = contribution.amounts.isDefined &&
                                         contribution.amounts.get.moneyPurchase.isDefined &&
                                         !contribution.amounts.get.definedBenefit.isDefined

  protected lazy val isGroup3: Boolean = contribution.amounts.isDefined &&
                                         contribution.isTriggered &&
                                         contribution.amounts.get.moneyPurchase.isDefined &&
                                         contribution.amounts.get.definedBenefit.isDefined

  protected lazy val _alternativeChargableAmount =
    if (isMPAAApplicable && isTriggered) {
      val v = mpist + dbist
      Logger.debug(s"ACA: ${mpist} + ${dbist} = $v")
      v
    }
    else {
      Logger.debug(s"ACA: 0")
      0L
    }

  protected lazy val _isMPAAApplicable = {
    Logger.debug(s"isMPA: ${isTriggered} && ${definedContribution} > ${moneyPurchaseAA}")
    isTriggered && definedContribution > moneyPurchaseAA
  }

  protected lazy val _mpa = config.get("mpaa").getOrElse(DEFAULT_MPA) * 100L

  protected lazy val _preTriggerSavings =
    previousPeriods.find{
      (r)=>
      r.input.taxPeriodStart.taxYear == contribution.taxPeriodStart.taxYear && !r.input.isTriggered
    }.map{
      (r)=>
      Logger.debug(s"PTS: ${r.input.definedBenefit} + ${r.input.moneyPurchase}")
      r.input.definedBenefit + r.input.moneyPurchase
    }.getOrElse {
      if (!isTriggered) {
        Logger.debug(s"PTS (nte): ${_definedBenefit}")
        _definedBenefit
      } else {
        Logger.warn(s"PTS: 0")
        0L
      }
    }

  protected lazy val _unusedMPAA =
    if (isTriggered && !isMPAAApplicable) {
      moneyPurchaseAA - definedContribution
    } else {
      0L
    }

  protected lazy val _unusedAAA =
    if (isMPAAApplicable) {
      (alternativeAA - definedBenefit).max(0)
    } else {
      0L
    }

  protected lazy val _previousAvailableAAWithCCF =
    previousYear.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  protected lazy val _dbist =
    if (isMPAAApplicable) {
      Logger.debug(s"DBIST(mpa): ${definedBenefit} - (${alternativeAA} + ${_previousAvailableAAWithCCF})")
      (definedBenefit - (alternativeAA + _previousAvailableAAWithCCF)).max(0)
    } else {
      Logger.debug(s"DBIST: ${preFlexiSavings} - ${_previousAvailableAAWithCCF}")
      (preFlexiSavings - _previousAvailableAAWithCCF).max(0)
    }

  protected lazy val _mpist =
    if (isMPAAApplicable) {
      val v = (definedContribution - moneyPurchaseAA).max(0)
      Logger.debug(s"MPIST(mpa): ${definedContribution} - ${moneyPurchaseAA} = ${v}")
      v
    } else {
      Logger.debug(s"MPIST: 0")
      0L
    }

  protected lazy val _taa = config.get("taa").getOrElse(DEFAULT_TAA) * 100L

  protected lazy val _taperStart = config.get("taperStart").getOrElse(DEAFULT_TAPER_START) * 100L

  protected lazy val _taperEnd = config.get("taperEnd").getOrElse(DEAFULT_TAPER_END) * 100L

  protected lazy val _annualAllowance: Long = config.get("annual").getOrElse(DEAFULT_AA) * 100L

  // Is ACA Applicable
  protected lazy val _isACA = isTriggered && alternativeChargableAmount > defaultChargableAmount
}
// scalastyle:on number.of.methods

case class Post2015TaperedAllowanceCalculator(implicit previousPeriods:Seq[TaxYearResults],
                                                       contribution:Contribution)
  extends TaperedAllowanceCalculator {
}

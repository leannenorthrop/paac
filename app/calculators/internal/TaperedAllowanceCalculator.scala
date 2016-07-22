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

import calculators._
import calculators.internal.Utilities._
import models._
import config.PaacConfiguration

trait TaperedAllowanceCalculator extends ExtendedSummaryCalculator {
  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  override def acaCF() : Long = _acaCF
  override def allowance(): Long = _annualAllowance
  override def alternativeAA(): Long = _alternativeAA
  override def alternativeChargableAmount(): Long = _alternativeChargableAmount
  override def annualAllowance(): Long = _taperedAllowance
  override def annualAllowanceCF(): Long = _annualAllowanceCF
  override def annualAllowanceCCF(): Long = _annualAllowanceCCF
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
  override def isMPAAApplicable(): Boolean = _isMPAAApplicable
  override def moneyPurchaseAA(): Long = _mpa
  override def mpist(): Long = _mpist
  override def postFlexiSavings(): Long = _postFlexiSavings  
  override def preFlexiSavings(): Long = _preTriggerSavings
  override def unusedAAA(): Long = _unusedAAA
  override def unusedAllowance(): Long = _unusedAllowance
  override def unusedMPAA(): Long = _unusedMPAA

  protected def basicCalculator(): SummaryCalculator = 
    BasicAllowanceCalculator((_annualAllowance/100D).toInt, previousPeriods, contribution)

  protected def isTaperingApplicable(): Boolean = 
    contribution.amounts.flatMap(_.income.map(_ > _taperStart)).getOrElse(false)

  protected def isTriggered(): Boolean = contribution.isTriggered

  protected lazy val actualUnused = actualUnusedList(this)(previousPeriods,contribution)

  protected lazy val config: Map[String,Int] = 
    PaacConfiguration.forYear(contribution.taxPeriodStart.taxYear)

  protected lazy val previousYear = 
    previousPeriods.find(isYear(contribution.taxPeriodStart.taxYear-1))

  protected lazy val previous3YearsUnusedAllowance: Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(contribution.taxPeriodStart.taxYear, Some(InputAmounts(0L,0L)))
    val calc = BasicAllowanceCalculator(0,previousPeriods,c)
    actualUnusedList(calc)(previousPeriods, c).drop(1).slice(0,3).foldLeft(0L)(_+_._2)
  }

  protected lazy val _acaCF = 0L

  protected lazy val _dcaCF = 0L

  protected lazy val _postFlexiSavings = 
    if (isTriggered) 
      definedContribution + definedBenefit 
    else 
      0L

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

  protected lazy val _exceedingMPAA = 0L

  protected lazy val _exceedingAAA = 0L

  protected lazy val _defaultChargableAmount = 
    if (!isTriggered) 
      0L
    else
      (postFlexiSavings - (annualAllowance + previous3YearsUnusedAllowance)).max(0L)

  protected lazy val _annualAllowanceCCF = 0L

  protected lazy val _annualAllowanceCF = 0L

  protected lazy val _unusedAllowance = 
    if (isMPAAApplicable)
      0L
    else if (isTriggered) { 
       val savings = if (defaultChargableAmount >= alternativeChargableAmount) 
                       preFlexiSavings + definedContribution
                     else 
                       preFlexiSavings
       if (savings > annualAllowance) 
         0L 
       else 
         annualAllowance - savings
     }.max(0)
    else
      basicCalculator.unusedAllowance


  protected lazy val _exceedingAllowance = 0L

  // taper automatically applied as part of annualAllowance calculation
  protected lazy val _alternativeAA = (annualAllowance - moneyPurchaseAA).max(0L)

  protected lazy val _definedBenefit = 
    if (isTriggered) 
      contribution.definedBenefit + preFlexiSavings 
    else 
      contribution.definedBenefit + contribution.moneyPurchase

  protected lazy val _definedContribution = contribution.moneyPurchase

  protected lazy val _chargableAmount = 
    if (!isTriggered) basicCalculator.chargableAmount 
    else if (isMPAAApplicable) alternativeChargableAmount.max(defaultChargableAmount) 
    else defaultChargableAmount

  protected lazy val _taperedAllowance = 
    if (isTaperingApplicable) 
      contribution.amounts.flatMap(_.income.map {
        (ai)=>
        ai match {
          case income if income > _taperStart && income < _taperEnd => {
            val reduction = Math.floor(((ai - _taperStart)/100L)/2D)*100L
            (_annualAllowance - reduction).toLong
          }
          case income if income > _taperEnd => _taa
          case _ => _annualAllowance
        }
      }).getOrElse(_annualAllowance)
    else _annualAllowance

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
    if (isMPAAApplicable && isTriggered) 
      mpist + dbist 
    else 
      0L

  protected lazy val _isMPAAApplicable = definedContribution > moneyPurchaseAA

  protected lazy val _mpa = config.get("mpaa").getOrElse(10000) * 100L

  protected lazy val _preTriggerSavings = 
    preTriggerInputs(previousPeriods).map((c)=>c.definedBenefit+c.moneyPurchase).getOrElse(0L)

  protected lazy val _unusedMPAA = 
    if (isTriggered && !isMPAAApplicable) 
      moneyPurchaseAA - definedContribution 
    else 
      0L  

  protected lazy val _unusedAAA = 
    if (isMPAAApplicable && isTriggered) 
      (alternativeAA - definedBenefit).max(0) 
    else 
      0L

  protected lazy val _previousAvailableAAWithCCF = 
    previousYear.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  protected lazy val _dbist =
    if (isTriggered)
      (
        definedBenefit-
        (preTriggerFields(previousPeriods).map(_.unusedAAA).getOrElse(0L) + 
         _previousAvailableAAWithCCF)
      ).max(0)
    else
      (preFlexiSavings - _previousAvailableAAWithCCF).max(0)

  protected lazy val _mpist = 
    if (isTriggered) 
      if (isMPAAApplicable) 
        (definedContribution - moneyPurchaseAA).max(0) 
      else 
        0L
    else 
      0L

  protected lazy val _taa = config.get("taa").getOrElse(10000) * 100L
  
  protected lazy val _taperStart = config.get("taperStart").getOrElse(150000) * 100L
  
  protected lazy val _taperEnd = config.get("taperEnd").getOrElse(210000) * 100L
  
  protected lazy val _annualAllowance: Long = config.get("annual").getOrElse(40000) * 100L
}

case class Post2015TaperedAllowanceCalculator(implicit previousPeriods:Seq[TaxYearResults], 
                                                       contribution:Contribution) 
  extends TaperedAllowanceCalculator {
}
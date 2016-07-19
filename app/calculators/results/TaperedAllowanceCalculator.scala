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

package calculators.results

import calculators._
import calculators.Utilities._
import models._
import config.PaacConfiguration

case class TaperedAllowanceCalculator(implicit previousPeriods:Seq[TaxYearResults], 
                                               contribution:Contribution) extends ExtendedSummaryCalculator {

  protected lazy val config: Map[String,Int] = PaacConfiguration.forYear(contribution.taxPeriodStart.taxYear)

  def allowance(): Long = _annualAllowance

  def definedBenefit(): Long = 0L

  def definedContribution(): Long = contribution.moneyPurchase

  protected lazy val _taa = config.get("taa").getOrElse(10000) * 100L
  protected lazy val _taperStart = config.get("taperStart").getOrElse(150000) * 100L
  protected lazy val _taperEnd = config.get("taperEnd").getOrElse(210000) * 100L
  protected lazy val _annualAllowance: Long = config.get("annual").getOrElse(40000) * 100L

  def annualAllowance(): Long = if (isTaperingApplicable) 
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

  override def exceedingAllowance(): Long = 0L

  override def unusedAllowance(): Long = 0L

  override def annualAllowanceCF(): Long = 0L

  override def annualAllowanceCCF(): Long = 0L

  override def chargableAmount(): Long = 0L

  protected lazy val _mpa = config.get("mpaa").getOrElse(10000) * 100L

  override def moneyPurchaseAA(): Long = _mpa
  
  override def alternativeAA(): Long = (annualAllowance - moneyPurchaseAA).max(0L)
  
  override def dbist(): Long = 0L
  
  override def mpist(): Long = 0L
  
  override def alternativeChargableAmount(): Long = 0L
  
  override def defaultChargableAmount(): Long = 0L
  
  override def cumulativeMP(): Long = 0L
  
  override def cumulativeDB(): Long = 0L
  
  override def exceedingMPAA(): Long = 0L
  
  override def exceedingAAA(): Long = 0L
  
  override def unusedAAA(): Long = 0L

  protected lazy val _unusedMPAA = if (isTriggered && !isMPAAApplicable) moneyPurchaseAA - definedContribution else 0L  
  override def unusedMPAA(): Long = _unusedMPAA
  
  override def preFlexiSavings(): Long = 0L
  
  override def postFlexiSavings(): Long = 0L
  
  protected lazy val _isMPAAApplicable = definedContribution > moneyPurchaseAA
  override def isMPAAApplicable(): Boolean = _isMPAAApplicable
  
  override def acaCF() : Long = 0L
  
  override def dcaCF() : Long = 0L

  protected def isTaperingApplicable(): Boolean = contribution.amounts.flatMap(_.income.map(_ > _taperStart)).getOrElse(false)
}

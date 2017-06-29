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

/**
 * Basic calculator, used for calculations for pension periods prior to 2015.
 * All values are in pence unless stated otherwise.
 * In need of refactoring to simplify if else blocks into separated concerns.
 */
trait SimpleAllowanceCalculator extends SummaryCalculator with DetailsCalculator {

  /** Abstract method to provide allowance as whole pounds value. */
  def annualAllowanceInPounds(): Long

  /** Abstract method to provide inputs and results from previous pension periods. */
  def previousPeriods(): Seq[TaxYearResults]

  /** Pension contributions for this pension period. */
  def contribution(): Contribution

  /** Annual Pension ALlowance in pounds */
  def allowance(): Long = annualAllowanceInPounds

  /**
   *  Prior to 2015 combined Defined Contribution and
   *  Defined Benefit Contributions. If used after 2015
   *  then also combined DC and DB savings unless triggered
   *  in which case only the DB.
   */
  protected lazy val _definedBenefit = {
    if (sumDbAndDc) {
      val v = contribution.definedBenefit + definedContribution
      detail("savings.db.calculation",s"db:${currency(contribution.definedBenefit)};op: + ;mp:${currency(definedContribution)};")
      v
    }
    else {
      detail("savings.db.calculation",s"db:${currency(contribution.definedBenefit)};")
      contribution.definedBenefit
    }
  }
  def sumDbAndDc(): Boolean = (contribution.taxPeriodStart.taxYear < 2015 ||
                               contribution.taxPeriodStart.taxYear == 2015 && !contribution.isTriggered)

  /** Defined benefit for this pension period. */
  def definedBenefit(): Long = _definedBenefit

  /** Defined contribution for this pension period. */
  protected lazy val _definedContribution = {
    detail("savings.dc",s"mp:${currency(contribution.moneyPurchase)};")
    contribution.moneyPurchase
  }

  /** Defined contribution for this pension period */
  def definedContribution(): Long = _definedContribution

  /** Annual allowance in pence */
  protected lazy val _aa = {
    val v = annualAllowanceInPounds*100L
    detail("allowance.annual.result",s"aa:${currency(v)};")
    v
  }

  /** Annual allowance in pence */
  def annualAllowance(): Long = _aa

  /** Pension savings in excess of annaul allowance (unused in ui) */
  protected lazy val _exceedingAllowance = (definedBenefit - annualAllowance).max(0)
  def exceedingAllowance(): Long = _exceedingAllowance

  /** Annual allowance - savings (unused in ui) */
  protected lazy val _unusedAllowance = {
    val v = (annualAllowance - definedBenefit).max(0)
    detail("allowance.unused.cy.calculation",s"aa:${currency(annualAllowance)};op: - ;mp:${currency(definedBenefit)};")
    v
  }
  def unusedAllowance(): Long = _unusedAllowance

  /** Total allowances available for this pension period. */
  protected lazy val _annualAllowanceCF =
    previousPeriods.headOption match {
      case Some(lastYear) => {
        val allowanceCarriedForward = lastYear.summaryResult.availableAAWithCCF
        val v2 = allowanceCarriedForward + annualAllowance
        detail("allowance.cf.calculation",s"aaccf:${currency(allowanceCarriedForward)};op: + ;aa:${currency(annualAllowance)};")
        v2
      }
      case _ => {
        detail("allowance.cf.calculation",s"aa:${currency(annualAllowance)};")
        annualAllowance
      }
    }
  def annualAllowanceCF(): Long = _annualAllowanceCF

  /**
   * Cumulative carry forwards is 2 previous years plus current year's unused annual allowance to
   * become available for next's years allowances.
   */
  protected lazy val _annualAllowanceCCF = {
    val year = contribution.taxPeriodStart.taxYear
    ccfdetails(year, contribution, previousPeriods)(this,this)
    actualUnused(this)(3)(previousPeriods,contribution)
  }

  def annualAllowanceCCF(): Long = _annualAllowanceCCF

  protected lazy val _chargableAmount =
    if (isBefore2011) {
      detail("chargable.calculation",s"no_tax_pre_2011:no_tax_pre_2011;")
      -1
    } else {
      val v = (definedBenefit - annualAllowanceCF).max(0)
      detail("chargable.calculation",s"cs:${currency(definedBenefit)};op: - ;aaaa:${currency(annualAllowanceCF)};")
      v
    }

  protected def isBefore2011: Boolean = contribution.taxPeriodStart.year < 2011

  def chargableAmount(): Long =  _chargableAmount

  def summary(): Option[Summary] =
    contribution.amounts.map { _ =>
      SummaryResult(chargableAmount,
                    exceedingAllowance,
                    annualAllowance,
                    unusedAllowance,
                    annualAllowanceCF,
                    annualAllowanceCCF)
    }
}

case class BasicAllowanceCalculator(annualAllowanceInPounds: Long,
                                    previousPeriods:Seq[TaxYearResults],
                                    contribution: Contribution) extends SimpleAllowanceCalculator

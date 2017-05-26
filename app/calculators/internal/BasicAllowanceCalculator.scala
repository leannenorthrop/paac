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

trait SimpleAllowanceCalculator extends SummaryCalculator with DetailsCalculator {

  def annualAllowanceInPounds(): Long
  def previousPeriods(): Seq[TaxYearResults]
  def contribution(): Contribution

  def allowance(): Long = annualAllowanceInPounds

  protected lazy val _definedBenefit = {
    val year = contribution.taxPeriodStart.taxYear
    if (year < 2015 || year == 2015 && !contribution.isTriggered) {
      val v = contribution.definedBenefit + definedContribution
      detail("savings.db.calculation",s"db:${fmt(contribution.definedBenefit)};op:+;mp:${fmt(definedContribution)}")
      v
    }
    else {
      contribution.definedBenefit
    }
  }
  def definedBenefit(): Long = {
    detail("savings.db.result",fmt(_definedBenefit))
    _definedBenefit
  }

  protected lazy val _definedContribution = contribution.moneyPurchase

  def definedContribution(): Long = {
    detail("savings.dc.result",fmt(_definedContribution))
    _definedContribution
  }

  protected lazy val _aa = annualAllowanceInPounds*100L // convert allowance from pounds to pence
  def annualAllowance(): Long = {
    detail("allowance.annual.result",fmt(_aa))
    _aa
  }

  protected lazy val _exceedingAllowance = (definedBenefit - annualAllowance).max(0)
  def exceedingAllowance(): Long = _exceedingAllowance

  protected lazy val _unusedAllowance = {
    val v = (annualAllowance - definedBenefit).max(0)
    detail("allowance.unused.cy.calculation",s"aa:${fmt(annualAllowance)};op:-;mp:${fmt(definedBenefit)}")
    v
  }
  def unusedAllowance(): Long = {
    detail("allowance.unused.cy.result",fmt(_unusedAllowance))
    _unusedAllowance
  }

  // total annual allowance possible
  // LN TODO Update to consider 2015 2 periods if this is reused for 2016
  protected lazy val _annualAllowanceCF = {
    val v = previousPeriods.headOption match {
      case Some(lastYear) => {
        val v2 = lastYear.summaryResult.availableAAWithCCF + annualAllowance
        detail("allowance.cf.calculation",s"aaccf:${fmt(lastYear.summaryResult.availableAAWithCCF)};op:+;aa:${fmt(annualAllowance)}")
        v2
      }
      case _ => {
        detail("allowance.cf.calculation",s"aa:${fmt(annualAllowance)}")
        annualAllowance
      }
    }
    v
  }
  def annualAllowanceCF(): Long = {
    detail("allowance.cf.result",fmt(_annualAllowanceCF))
    _annualAllowanceCF
  }

  // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
  protected lazy val _annualAllowanceCCF =
    if (contribution.taxPeriodStart.year < 2011) {
      // Prior to 2011 nothing was liable for tax charge and carry forwards are allowed
      ccfdetails
      actualUnused(this)(3)(previousPeriods,contribution)
    } else {
      if (exceedingAllowance > 0) {
        val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())
        if (exceedingAllowance >= previousResults.availableAAWithCCF) {
          detail("allowance.ccf.calculation",s"eaa:${fmt(exceedingAllowance)};op:>=;prev.year.unusedcf:${fmt(previousResults.availableAAWithCCF)}")
          0L
        } else {
          ccfdetails
          actualUnused(this)(3)(previousPeriods,contribution)
        }
      } else {
        ccfdetails
        actualUnused(this)(3)(previousPeriods,contribution)
      }
    }
  def ccfdetails: Unit = {
    val desc = actualUnusedList(this)(previousPeriods,contribution).slice(0,3) match {
      case head :: Nil => s"unused_${head._1}:${fmt(head._2)}"
      case head :: tail => tail.foldLeft(s"unused_${head._1}:${fmt(head._2)};")((str,pair)=>str + s"op:+;unused_${pair._1}:${fmt(pair._2)};")
      case _ => ""
    }
    detail("allowance.ccf.calculation",desc)
  }

  def annualAllowanceCCF(): Long = {
    detail("allowance.ccf.result",fmt(_annualAllowanceCCF))
    _annualAllowanceCCF
  }

  protected lazy val _chargableAmount =
    if (contribution.taxPeriodStart.year < 2011) {
      detail("chargable.calculation",s"no_tax_pre_2011:no_tax_pre_2011")
      - 1
    } else {
      val v = (definedBenefit - annualAllowanceCF).max(0)
      detail("chargable.calculation",s"db:${fmt(definedBenefit)};op:-;unusedcf:${fmt(annualAllowanceCF)}")
      v
    }
  def chargableAmount(): Long = {
    detail("chargable.result",fmt(_chargableAmount))
    _chargableAmount
  }

  def summary(): Option[Summary] =
    contribution.amounts.map {
      _ =>
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

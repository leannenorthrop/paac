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

package models

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import models.PensionPeriod._

/**
  Contribution representing pension savings for a single pension period.
*/
trait CalculationParam {
  def taxPeriodStart(): PensionPeriod
  def taxPeriodEnd(): PensionPeriod
  def amounts(): Option[InputAmounts]

  /**
    Helper function for front-end. Should be converted to use messages.
  */
  def taxYearLabel() : String =
    if (taxPeriodStart.year == YEAR_2015 && taxPeriodStart.month == JULY ||
        taxPeriodEnd.year == (YEAR_2015 + 1) && taxPeriodEnd.month == APRIL) {
      s"2015/16 P2"
    } else {
      if (taxPeriodStart.year == YEAR_2015 && taxPeriodStart.month == APRIL ||
             taxPeriodEnd.year == YEAR_2015 && taxPeriodEnd.month == JULY) {
        s"2015/16 P1"
      } else {
        s"${taxPeriodStart.year}/${taxPeriodEnd.year.toString().drop(2)}"
      }
    }

  /**
    Helper function for tests.
  */
  def label() : String = {
    val beforeAfter = if (amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)) "A" else "B"
    if (isPeriod2) {
      s"15/16 P2 $beforeAfter"
    } else if (isPeriod1) {
      s"15/16 P1 $beforeAfter"
    } else {
      s"${taxPeriodStart.year.toString().drop(2)}/${taxPeriodEnd.year.toString().drop(2)}   "
    }
  }

  def isEmpty(): Boolean =
    !amounts.isDefined || (amounts.isDefined && amounts.get.isEmpty)

  def isPeriod1(): Boolean =
    taxPeriodStart.isPeriod1 && taxPeriodEnd.isPeriod1

  def isPeriod2(): Boolean =
    taxPeriodStart.isPeriod2 && taxPeriodEnd.isPeriod2

  def isGroup1(): Boolean =
    amounts.isDefined &&
    (isPeriod1() || isPeriod2()) &&
    (!amounts.get.moneyPurchase.isDefined && !isTriggered)

  def isGroup2(): Boolean =
    amounts.isDefined &&
    (isPeriod1() || isPeriod2()) &&
    amounts.get.moneyPurchase.isDefined

  def isGroup3(): Boolean =
    amounts.isDefined &&
    (isPeriod2() &&
    isTriggered &&
    amounts.get.moneyPurchase.isDefined &&
    amounts.get.definedBenefit.isDefined)

  def isTriggered(): Boolean =
    (for {
      inputs <- amounts
      triggered <- inputs.triggered
    } yield triggered) getOrElse false

  def definedBenefit(): Long =
    (for {
      inputs <- amounts
      definedBenefit <- inputs.definedBenefit
    } yield definedBenefit) getOrElse 0L

  def moneyPurchase(): Long =
    (for {
      inputs <- amounts
      moneyPurchase <- inputs.moneyPurchase
    } yield moneyPurchase) getOrElse 0L

  def income(): Long =
    (for {
      inputs <- amounts
      income <- inputs.income
    } yield income) getOrElse 0L
}

case class Contribution(taxPeriodStart: PensionPeriod, taxPeriodEnd: PensionPeriod, amounts: Option[InputAmounts]) extends CalculationParam {
  // scalastyle:off
  def + (that:Contribution): Contribution =
    if (amounts.isDefined && that.amounts.isDefined) {
      this.copy(amounts=Some(InputAmounts((definedBenefit + that.definedBenefit),(moneyPurchase + that.moneyPurchase),(income + that.income))))
    } else {
      this
    }
  // scalastyle:on
}

object Contribution {
  implicit val contributionWrites: Writes[Contribution] = (
    (JsPath \ "taxPeriodStart").write[PensionPeriod] and
    (JsPath \ "taxPeriodEnd").write[PensionPeriod] and
    (JsPath \ "amounts").write[Option[InputAmounts]]
  )(unlift(Contribution.unapply))

  implicit val contributionReads: Reads[Contribution] = (
    (JsPath \ "taxPeriodStart").read[PensionPeriod] and
    (JsPath \ "taxPeriodEnd").read[PensionPeriod] and
    (JsPath \ "amounts").readNullable[InputAmounts]
  )(Contribution.apply(_:PensionPeriod, _:PensionPeriod, _:Option[InputAmounts]))

  /**
    Simplified apply function
  */
  def apply(year: Int, definedBenefit: Long) : Contribution = {
    // month is 0 based
    Contribution(PensionPeriod(year, APRIL, TAX_YEAR_START_DAY), PensionPeriod(year + 1, APRIL, TAX_YEAR_START_DAY - 1), Some(InputAmounts(definedBenefit)))
  }

  /**
    Simplified apply function
  */
  def apply(year: Int, amounts: Option[InputAmounts]) : Contribution = {
    // month is 0 based
    Contribution(PensionPeriod(year, APRIL, TAX_YEAR_START_DAY), PensionPeriod(year + 1, APRIL, TAX_YEAR_START_DAY - 1), amounts)
  }

  def apply(start: PensionPeriod, end: PensionPeriod, db: Long, dc: Long, triggered: Boolean): Contribution = {
    val amounts = InputAmounts(db, dc).copy(triggered=Some(triggered))
    Contribution(start, end, Some(amounts))
  }

  /**
    Simplified apply function
  */
  def apply(isP1: Boolean, db: Long, dc: Long): Contribution = {
    val start = if (isP1) PensionPeriod.PERIOD_1_2015_START else PensionPeriod.PERIOD_2_2015_START
    val end = if (isP1) PensionPeriod.PERIOD_1_2015_END else PensionPeriod.PERIOD_2_2015_END
    Contribution(start, end, db, dc, false)
  }

  /**
    Returns 'annual' available allowance for period 1 if given true, and period 2 if given false
  */
  def periodAllowance(isP1:Boolean):Long = {
    val c = Contribution(isP1, 0, 0)
    calculators.results.Calculator(c).allowance(c)
  }

  /**
    Returns annual available allowance for given year.
    For 2015 use 20151 for period 1 and 20152 for period 2.
  */
  def allowance(year:Int):Long =
    if (year == 20151) {
      periodAllowance(true)
    } else if (year == 20152) {
      periodAllowance(false)
    } else {
      val c = Contribution(year,0L)
      calculators.results.Calculator(c).allowance(c)
    }

  /**
    Helper function to sort pension contributions by pension period start date, considering periods 1 and 2 for 2015.
  */
  def sortByYearAndPeriod(left: Contribution, right: Contribution): Boolean = {
    if (left.taxPeriodStart.year == 2015 &&
        right.taxPeriodStart.year == 2015) {
      left.isPeriod1 && right.isPeriod2 ||
      (left.isPeriod1 && right.isPeriod1 && !left.amounts.get.triggered.get) ||
      (left.isPeriod2 && right.isPeriod2 && !left.amounts.get.triggered.get)
    } else {
      left.taxPeriodStart < right.taxPeriodStart
    }
  }
}

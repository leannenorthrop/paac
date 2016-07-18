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

sealed trait CalculationParam
sealed trait PensionCalculatorValue {
  def isEmpty(): Boolean
  def definedBenefit():  Option[Long]
  def moneyPurchase():  Option[Long]
  def income(): Option[Long]
}

/**
  Calculator inputs for a single year
 */
case class InputAmounts(definedBenefit: Option[Long] = None, 
                        moneyPurchase: Option[Long] = None, 
                        income: Option[Long] = None,
                        triggered: Option[Boolean] = None) extends PensionCalculatorValue {
  def isEmpty() : Boolean = {
    !definedBenefit.isDefined && !moneyPurchase.isDefined && !income.isDefined
  }
}

/**
  Simple date object representing a point in time within a pension period.
*/
case class PensionPeriod(year: Int, month: Int, day: Int) {
  def <(that: PensionPeriod): Boolean = if (year == that.year && month == that.month && day == that.day) false else 
                                    year < that.year || (year == that.year && month < that.month) || (year == that.year && month == that.month && day < that.day) 
  def >(that: PensionPeriod): Boolean = if (year == that.year && month == that.month && day == that.day) false else 
                                    year > that.year || (year == that.year && month > that.month) || (year == that.year && month == that.month && day > that.day) 
  def <=(that: PensionPeriod): Boolean = if (year == that.year && month == that.month && day == that.day) true else this < that
  def >=(that: PensionPeriod): Boolean = if (year == that.year && month == that.month && day == that.day) true else this > that
  def isPeriod(s:PensionPeriod, e:PensionPeriod):Boolean = {
    (this >= s) && (this <= e)
  }

  def isPeriod1(): Boolean = {
    isPeriod(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END)
  }

  def isPeriod2(): Boolean = {
    isPeriod(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END)
  }
}

/**
  Contribution representing pension savings for a single pension period.
*/
case class Contribution(taxPeriodStart: PensionPeriod, taxPeriodEnd: PensionPeriod, amounts: Option[InputAmounts]) extends CalculationParam {
  /**
    Helper function for front-end. Should be converted to use messages.
  */
  def taxYearLabel() : String = {
    if (taxPeriodStart.year == 2015 && taxPeriodStart.month == 7 ||
        taxPeriodEnd.year == 2016 && taxPeriodEnd.month == 4) {
      s"2015/16 P2"  
    } else if (taxPeriodStart.year == 2015 && taxPeriodStart.month == 4 ||
               taxPeriodEnd.year == 2015 && taxPeriodEnd.month == 7) {
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
  
  def isEmpty(): Boolean = {
    !amounts.isDefined || (amounts.isDefined && amounts.get.isEmpty)
  }

  def isPeriod1(): Boolean = {
    taxPeriodStart.isPeriod1 && taxPeriodEnd.isPeriod1
  }

  def isPeriod2(): Boolean = {
    taxPeriodStart.isPeriod2 && taxPeriodEnd.isPeriod2
  }

  def + (that:Contribution): Contribution = {
    if (amounts.isDefined && that.amounts.isDefined) {
      val thisAmounts = amounts.get
      val thatAmounts = that.amounts.get
      val db = thisAmounts.definedBenefit.map((v:Long)=>v+thatAmounts.definedBenefit.getOrElse(0L)).getOrElse(thatAmounts.definedBenefit.getOrElse(0L))
      val dc = thisAmounts.moneyPurchase.map((v:Long)=>v+thatAmounts.moneyPurchase.getOrElse(0L)).getOrElse(thatAmounts.moneyPurchase.getOrElse(0L))
      this.copy(amounts=Some(InputAmounts(db,dc)))
    } else {
      this
    }
  }

  def isGroup1(): Boolean = {
    amounts.isDefined && 
    (isPeriod1() || isPeriod2()) &&
    ( !amounts.get.moneyPurchase.isDefined &&
     ( !amounts.get.triggered.isDefined|| !amounts.get.triggered.get))
  }

  def isGroup2(): Boolean = {
    amounts.isDefined && 
    (isPeriod1() || isPeriod2()) && 
    amounts.get.moneyPurchase.isDefined
  }

  def isGroup3(): Boolean = {
    amounts.isDefined && 
    (isPeriod2() && 
    amounts.get.triggered.getOrElse(false) && 
    amounts.get.moneyPurchase.isDefined &&
    amounts.get.definedBenefit.isDefined)
  }

  def isTriggered(): Boolean = {
    (for {
      inputs <- amounts
      triggered <- inputs.triggered
    } yield triggered) getOrElse false
  }

  def definedBenefit(): Long = {
    (for {
      inputs <- amounts
      definedBenefit <- inputs.definedBenefit
    } yield definedBenefit) getOrElse 0L
  }
  
  def moneyPurchase(): Long = {
    (for {
      inputs <- amounts
      moneyPurchase <- inputs.moneyPurchase
    } yield moneyPurchase) getOrElse 0L
  }
}

object PensionPeriod {
  // Unlike front-end backend must have fixed supported start and end years
  // as calculation rules are very dependant on a varying set of rules for each year
  val EARLIEST_YEAR_SUPPORTED:Int = 2008
  val LATEST_YEAR_SUPPORTED:Int = 2100

  // Constants for pension period json validation
  val MIN_MONTH_VALUE:Int = 1
  val MIN_DAY_VALUE:Int = 1
  val MAX_MONTH_VALUE:Int = 12
  val MAX_DAY_VALUE:Int = 31

  // Helpers for constructing pension contributions
  val PERIOD_1_2015_START = PensionPeriod(2015, 4, 6)
  val PERIOD_1_2015_END = PensionPeriod(2015, 7, 8)
  val PERIOD_2_2015_START = PensionPeriod(2015, 7, 9)
  val PERIOD_2_2015_END = PensionPeriod(2016, 4, 5)  

  implicit val taxPeriodWrites: Writes[PensionPeriod] = (
    (JsPath \ "year").write[Int] and
    (JsPath \ "month").write[Int] and
    (JsPath \ "day").write[Int]
  )(unlift(PensionPeriod.unapply))

  implicit val taxPeriodReads: Reads[PensionPeriod] = (
    (JsPath \ "year").read[Int](min(EARLIEST_YEAR_SUPPORTED)) and
    (JsPath \ "month").read[Int](min(MIN_MONTH_VALUE) keepAnd max(MAX_MONTH_VALUE)) and
    (JsPath \ "day").read[Int](min(MIN_DAY_VALUE) keepAnd max(MAX_DAY_VALUE))
  )(PensionPeriod.apply _)
}

object InputAmounts {
  implicit val inputAmountsWrites: Writes[InputAmounts] = (
    (JsPath \ "definedBenefit").write[Option[Long]] and
    (JsPath \ "moneyPurchase").write[Option[Long]] and
    (JsPath \ "income").write[Option[Long]] and
    (JsPath \ "triggered").write[Option[Boolean]]
  )(unlift(InputAmounts.unapply))

  implicit val inputAmountsReads: Reads[InputAmounts] = (
    (JsPath \ "definedBenefit").readNullable[Long](min(0L)) and
    (JsPath \ "moneyPurchase").readNullable[Long](min(0L)) and
    (JsPath \ "income").readNullable[Long](min(0L)) and
    (JsPath \ "triggered").readNullable[Boolean]
  )(InputAmounts.apply(_: Option[Long], _: Option[Long], _: Option[Long], _: Option[Boolean]))

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long, moneyPurchase: Long, income: Long) : InputAmounts = {
    InputAmounts(Some(definedBenefit), Some(moneyPurchase), Some(income), None)
  }

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long, moneyPurchase: Long) : InputAmounts = {
    InputAmounts(Some(definedBenefit), Some(moneyPurchase), None, None)
  }

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long) : InputAmounts = {
    InputAmounts(Some(definedBenefit), None, None, None)
  }
}

object Contribution {
  import calculators.Utilities._

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
    Implicit cast function from Contribution to SummaryResultsTuple.
    Used when calculating actual unused allowance.
  */
  implicit def convert(c: Contribution)(implicit calculator:calculators.SummaryCalculator): SummaryResultsTuple = {
    implicit val contribution = c
    contribution match {
      case _ if c.isPeriod1 || c.isPeriod2 => (2015, calculator.exceedingAllowance, calculator.unusedAllowance)
      case _ => (contribution.taxPeriodStart.year, calculator.exceedingAllowance, calculator.unusedAllowance)
    }
  }

  /**
    Simplified apply function
  */
  def apply(year: Int, definedBenefit: Long) : Contribution = {
    // month is 0 based
    Contribution(PensionPeriod(year, 4, 6), PensionPeriod(year + 1, 4, 5), Some(InputAmounts(definedBenefit)))
  }

  /**
    Simplified apply function
  */
  def apply(year: Int, amounts: Option[InputAmounts]) : Contribution = {
    // month is 0 based
    Contribution(PensionPeriod(year, 4, 6), PensionPeriod(year + 1, 4, 5), amounts)
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
  def periodAllowance(isP1:Boolean):Long = calculators.Calculator(Contribution(isP1, 0, 0)).allowance

  /**
    Returns annual available allowance for given year.
    For 2015 use 20151 for period 1 and 20152 for period 2.
  */
  def allowance(year:Int):Long = if (year == 20151) periodAllowance(true) else if (year == 20152) periodAllowance(false) else calculators.Calculator(Contribution(year,0L)).allowance

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

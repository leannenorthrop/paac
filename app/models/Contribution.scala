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

import java.util.GregorianCalendar

sealed trait CalculationParam
sealed trait PensionCalculatorValue

case class InputAmounts(definedBenefit: Option[Long] = None, moneyPurchase: Option[Long] = None) extends PensionCalculatorValue {
  def isEmpty() : Boolean = {
    definedBenefit == None && moneyPurchase == None
  }
}

/** TaxPeriod really aught to be named SimpleDate.
    Follows java.util.Calendar so that month is 0 based,
    but year and day operate as expected.*/
case class TaxPeriod(year: Int, month: Int, day: Int) {
  def toCalendar() : GregorianCalendar = new GregorianCalendar(year, month, day)
}

case class Contribution(taxPeriodStart: TaxPeriod, taxPeriodEnd: TaxPeriod, amounts: Option[InputAmounts]) extends CalculationParam {
  def taxYearLabel() : String = s"${taxPeriodStart.year}/${taxPeriodEnd.year.toString().drop(2)}"
  def isEmpty() : Boolean = {
    amounts == None || (amounts.isDefined && amounts.get.isEmpty)
  }
}

object TaxPeriod {
  // Unlike front-end backend must have fixed supported start and end years
  // as calculation rules are very dependant on a varying set of rules for each year
  val EARLIEST_YEAR_SUPPORTED:Int = 2006
  val LATEST_YEAR_SUPPORTED:Int = 2016

  val MIN_VALUE:Int = 0
  val MIN_DAY_VALUE:Int = 1

  implicit val taxPeriodWrites: Writes[TaxPeriod] = (
    (JsPath \ "year").write[Int] and
    (JsPath \ "month").write[Int] and
    (JsPath \ "day").write[Int]
  )(unlift(TaxPeriod.unapply))

  implicit val taxPeriodReads: Reads[TaxPeriod] = (
    (JsPath \ "year").read[Int](min(EARLIEST_YEAR_SUPPORTED)) and
    (JsPath \ "month").read[Int](min(MIN_VALUE)) and
    (JsPath \ "day").read[Int](min(MIN_DAY_VALUE))
  )(TaxPeriod.apply _)
}

object InputAmounts {
  implicit val inputAmountsWrites: Writes[InputAmounts] = (
    (JsPath \ "definedBenefit").write[Option[Long]] and
    (JsPath \ "moneyPurchase").write[Option[Long]]
  )(unlift(InputAmounts.unapply))

  implicit val inputAmountsReads: Reads[InputAmounts] = (
    (JsPath \ "definedBenefit").readNullable[Long](min(0L)) and
    (JsPath \ "moneyPurchase").readNullable[Long](min(0L))
  )(InputAmounts.apply(_: Option[Long], _: Option[Long]))

  def apply(definedBenefit: Long, moneyPurchase: Long) : InputAmounts = {
    InputAmounts(Some(definedBenefit), Some(moneyPurchase))
  }

  def apply(definedBenefit: Long) : InputAmounts = {
    InputAmounts(Some(definedBenefit), None)
  }
}

object Contribution {
  implicit val contributionWrites: Writes[Contribution] = (
    (JsPath \ "taxPeriodStart").write[TaxPeriod] and
    (JsPath \ "taxPeriodEnd").write[TaxPeriod] and
    (JsPath \ "amounts").write[Option[InputAmounts]]
  )(unlift(Contribution.unapply))

  implicit val contributionReads: Reads[Contribution] = (
    (JsPath \ "taxPeriodStart").read[TaxPeriod] and
    (JsPath \ "taxPeriodEnd").read[TaxPeriod] and
    (JsPath \ "amounts").readNullable[InputAmounts]
  )(Contribution.apply(_:TaxPeriod, _:TaxPeriod, _:Option[InputAmounts]))

  def apply(year: Int, definedBenefit: Long) : Contribution = {
    // month is 0 based
    Contribution(TaxPeriod(year, 3, 6), TaxPeriod(year+1, 3, 5), Some(InputAmounts(definedBenefit=Some(definedBenefit))))
  }
}
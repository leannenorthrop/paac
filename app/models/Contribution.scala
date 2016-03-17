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
sealed trait PensionCalculatorValue

case class InputAmounts(definedBenefit: Long = 0, moneyPurchase: Long = 0) extends PensionCalculatorValue
case class TaxPeriod(year: Int, month: Int, day: Int)
case class Contribution(taxPeriodStart: TaxPeriod, taxPeriodEnd: TaxPeriod, amounts: InputAmounts) extends CalculationParam {
  def taxYearLabel() : String = s"${taxPeriodStart.year}/${taxPeriodEnd.year.toString().drop(2)}"
}

object TaxPeriod {
  val EARLIEST_YEAR_SUPPORTED:Int = 2008
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
    (JsPath \ "definedBenefit").write[Long] and
    (JsPath \ "moneyPurchase").write[Long]
  )(unlift(InputAmounts.unapply))

  implicit val inputAmountsReads: Reads[InputAmounts] = (
    (JsPath \ "definedBenefit").read[Long](min(0L)) and
    (JsPath \ "moneyPurchase").read[Long](min(0L))
  )(InputAmounts.apply _)
}

object Contribution {
  implicit val contributionWrites: Writes[Contribution] = (
    (JsPath \ "taxPeriodStart").write[TaxPeriod] and
    (JsPath \ "taxPeriodEnd").write[TaxPeriod] and
    (JsPath \ "amounts").write[InputAmounts]
  )(unlift(Contribution.unapply))

  implicit val contributionReads: Reads[Contribution] = (
    (JsPath \ "taxPeriodStart").read[TaxPeriod] and
    (JsPath \ "taxPeriodEnd").read[TaxPeriod] and
    (JsPath \ "amounts").read[InputAmounts]
  )(Contribution.apply(_:TaxPeriod, _:TaxPeriod, _:InputAmounts))

  def apply(year: Int, definedBenefit: Long) : Contribution = {
    Contribution(TaxPeriod(year, 4, 6), TaxPeriod(year+1, 4, 5), InputAmounts(definedBenefit=definedBenefit))
  }
}
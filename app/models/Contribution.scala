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
case class Contribution(taxYear: Short, amounts: InputAmounts) extends CalculationParam

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
  val EARLIEST_YEAR_SUPPORTED:Short = 2007
  implicit val contributionWrites: Writes[Contribution] = (
    (JsPath \ "taxYear").write[Short] and
    (JsPath \ "amounts").write[InputAmounts]
  )(unlift(Contribution.unapply))

  implicit val contributionReads: Reads[Contribution] = (
    (JsPath \ "taxYear").read[Short](min(EARLIEST_YEAR_SUPPORTED)) and
    (JsPath \ "amounts").read[InputAmounts]
  )(Contribution.apply _)
}
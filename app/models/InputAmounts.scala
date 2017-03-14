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

package models

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait PensionCalculatorValue {
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
  def isEmpty() : Boolean =
    !definedBenefit.isDefined && !moneyPurchase.isDefined && !income.isDefined
}

object InputAmounts {
  implicit val inputAmountsWrites: Writes[InputAmounts] = (
    (JsPath \ "definedBenefit").write[Option[Long]] and
    (JsPath \ "moneyPurchase").write[Option[Long]] and
    (JsPath \ "income").write[Option[Long]] and
    (JsPath \ "triggered").write[Option[Boolean]]
  )(unlift(InputAmounts.unapply))

  implicit val inputAmountsReads: Reads[InputAmounts] = (
    (JsPath \ "definedBenefit").readNullable[Long](min(0L) keepAnd max(1000000000L)) and
    (JsPath \ "moneyPurchase").readNullable[Long](min(0L) keepAnd max(1000000000L)) and
    (JsPath \ "income").readNullable[Long](min(0L) keepAnd max(1000000000L)) and
    (JsPath \ "triggered").readNullable[Boolean]
  )(InputAmounts.apply(_: Option[Long], _: Option[Long], _: Option[Long], _: Option[Boolean]))

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long, moneyPurchase: Long, income: Long) : InputAmounts =
    InputAmounts(Some(definedBenefit), Some(moneyPurchase), Some(income), None)

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long, moneyPurchase: Long) : InputAmounts =
    InputAmounts(Some(definedBenefit), Some(moneyPurchase), None, None)

  /**
    Simplified apply function
  */
  def apply(definedBenefit: Long) : InputAmounts =
    InputAmounts(Some(definedBenefit), None, None, None)
}

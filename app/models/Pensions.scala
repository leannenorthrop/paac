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
import play.api.libs.json.Writes._
import play.api.libs.json._

case class PensionInput(
                    taxYear: String,
                    pensionInputAmount: BigDecimal
                  )

object PensionInput {
  implicit val PensionWrites: Writes[PensionInput] = (
    (JsPath \ "taxYear").write[String] and
      (JsPath \ "pensionInputAmount").write[BigDecimal]
    ) (unlift(PensionInput.unapply))

  implicit val PensionReads: Reads[PensionInput] = (
    (JsPath \ "taxYear").read[String] and
      (JsPath \ "pensionInputAmount").read[BigDecimal]
    ) (PensionInput.apply _)
}

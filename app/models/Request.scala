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

sealed trait BackendRequest {
  def contributions(): List[Contribution]
  def startFromYear():  Option[Int]
  def missingYearsAreRegistered():  Option[Boolean]
}

case class CalculationRequest(contributions: List[Contribution], 
                              startFromYear: Option[Int] = None,
                              missingYearsAreRegistered: Option[Boolean] = None) extends BackendRequest {
}

object BackendRequest {
  implicit val requestWrites: Writes[BackendRequest] = (
    (JsPath \ "contributions").write[List[Contribution]] and
    (JsPath \ "startFromYear").write[Option[Int]] and
    (JsPath \ "missingYearsAreRegistered").write[Option[Boolean]]
  )(BackendRequest.toTuple _)

  implicit val requestReads: Reads[BackendRequest] = (
    (JsPath \ "contributions").read[List[Contribution]] and
    (JsPath \ "startFromYear").readNullable[Int](min(PensionPeriod.EARLIEST_YEAR_SUPPORTED)) and
    (JsPath \ "missingYearsAreRegistered").readNullable[Boolean]
  )(BackendRequest.toRequest _)

  def toTuple(request: BackendRequest): (List[Contribution], Option[Int], Option[Boolean]) = {
    (request.contributions, request.startFromYear, request.missingYearsAreRegistered)
  }

  def toRequest(contributions: List[Contribution],
                startFromYear: Option[Int],
                missingYearsAreRegistered: Option[Boolean]): BackendRequest = {
    CalculationRequest(contributions, startFromYear, missingYearsAreRegistered)
  }
}

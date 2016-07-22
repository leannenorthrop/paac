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

/**
  Container class for a single 'row' of calculation results 'table'.
  Originally intended to be expanded to include a detailed result object to 
  explain results as is the case with the original javascript based 
  extended calculator.
 */
case class TaxYearResults(input: Contribution = Contribution(2008,0L),
                          summaryResult: Summary = SummaryResult())

/** 
 TaxYearResults providing read/write for JSON and implicit casts.
 */
object TaxYearResults {
  implicit val summaryWrites: Writes[TaxYearResults] = (
    (JsPath \ "input").write[Contribution] and
    (JsPath \ "summaryResult").write[Summary] 
  )(unlift(TaxYearResults.unapply))

  implicit val summaryReads: Reads[TaxYearResults] = (
    (JsPath \ "input").read[Contribution] and
    (JsPath \ "summaryResult").read[Summary]
  )(TaxYearResults.apply _)
}
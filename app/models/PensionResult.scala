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

case class TaxYearResults(input: Contribution = Contribution(2008,0L),
                          summaryResult: Summary = SummaryResult())

trait Summary {
  def chargableAmount: Long
  def exceedingAAAmount: Long
  def availableAllowance: Long
  def unusedAllowance: Long
  def availableAAWithCF: Long    // total available allowance for current year should be renamed to totalAA
  def availableAAWithCCF: Long   // available allowance carried forward to following year
  def unusedAllowanceCF: Long
}

case class SummaryResult(chargableAmount: Long = 0,
                         exceedingAAAmount: Long = 0,
                         availableAllowance: Long = 0,
                         unusedAllowance: Long = 0,
                         availableAAWithCF: Long = 0,    // total available allowance for current year should be renamed to totalAA
                         availableAAWithCCF: Long = 0,   // available allowance carried forward to following year
                         unusedAllowanceCF: Long = 0) extends Summary

object Summary {
  implicit val summaryResultWrites: Writes[Summary] = (
    (JsPath \ "chargableAmount").write[Long] and
    (JsPath \ "exceedingAAAmount").write[Long] and 
    (JsPath \ "availableAllowance").write[Long] and
    (JsPath \ "unusedAllowance").write[Long] and 
    (JsPath \ "availableAAWithCF").write[Long] and
    (JsPath \ "availableAAWithCCF").write[Long] and
    (JsPath \ "unusedAllowanceCF").write[Long]
  )(Summary.toTuple _ )

  implicit val summaryResultReads: Reads[Summary] = (
    (JsPath \ "chargableAmount").read[Long] and
    (JsPath \ "exceedingAAAmount").read[Long] and
    (JsPath \ "availableAllowance").read[Long] and
    (JsPath \ "unusedAllowance").read[Long] and
    (JsPath \ "availableAAWithCF").read[Long] and
    (JsPath \ "availableAAWithCCF").read[Long] and
    (JsPath \ "unusedAllowanceCF").read[Long]
  )(Summary.toSummary _)

  def toTuple(summary: Summary): (Long, Long, Long, Long, Long, Long, Long) = {
    (summary.chargableAmount, summary.exceedingAAAmount, summary.availableAllowance, summary.unusedAllowance, summary.availableAAWithCF, summary.availableAAWithCCF, summary.unusedAllowanceCF)
  }

  def toSummary(chargableAmount: Long = 0,
                exceedingAAAmount: Long = 0,
                availableAllowance: Long = 0,
                unusedAllowance: Long = 0,
                availableAAWithCF: Long = 0,    // total available allowance for current year should be renamed to totalAA
                availableAAWithCCF: Long = 0,   // available allowance carried forward to following year
                unusedAllowanceCF: Long = 0): Summary = {
    SummaryResult(chargableAmount,
                  exceedingAAAmount,
                  availableAllowance,
                  unusedAllowance,
                  availableAAWithCF,
                  availableAAWithCCF,
                  unusedAllowanceCF)
  }
}

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

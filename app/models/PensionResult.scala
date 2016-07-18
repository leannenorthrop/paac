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
  Base trait for all summary result row objects. All amounts are in pence,
  therefore divide by 100 to get pounds and pence. 
*/
trait Summary {
  /** Tax due */
  def chargableAmount: Long     
  /** Amount exceeding annual allowance. Not always equal to chargable amount. */
  def exceedingAAAmount: Long
  /** Annual allowance available */
  def availableAllowance: Long
  /** Annual allowance that wasn't used and so available for carry forwards. */
  def unusedAllowance: Long
  /** total available allowance for current year should be renamed to totalAA */
  def availableAAWithCF: Long
  /** available allowance carried forward to following year */
  def availableAAWithCCF: Long 
  /** Alternative annual allowance that wasn't used. Only applicable from 2015 onwards. */
  def unusedAAA: Long
  /** Money purchase annual allowance amount not used. Only applicable when flexi-access event has occured. */
  def unusedMPAA: Long
  /** Amount exceeding MPA */
  def exceedingMPAA: Long
  /** Amount exceeding AAA */
  def exceedingAAA: Long
  /** True if mpa was applied */
  def isMPA: Boolean
  /** Money purchase annual allowance. */
  def moneyPurchaseAA: Long 
  /** Alternative annual allowance */
  def alternativeAA: Long
}

/** Simple summary result implementation. Used by calculators for years up to but not including 2015. */
case class SummaryResult(chargableAmount: Long = 0,
                         exceedingAAAmount: Long = 0,
                         availableAllowance: Long = 0,
                         unusedAllowance: Long = 0,
                         availableAAWithCF: Long = 0,
                         availableAAWithCCF: Long = 0,
                         unusedAAA: Long = 0,
                         unusedMPAA: Long = 0,
                         exceedingMPAA: Long = 0,
                         exceedingAAA: Long = 0,
                         isMPA: Boolean = false,
                         moneyPurchaseAA: Long = 0,
                         alternativeAA: Long = 0) extends Summary

/** Extends summary result implementation. Used by calculators for years from 2015 onwards. */
case class ExtendedSummaryFields(chargableAmount: Long = 0,
                                 exceedingAAAmount: Long = 0,
                                 availableAllowance: Long = 0,
                                 unusedAllowance: Long = 0,
                                 availableAAWithCF: Long = 0,
                                 availableAAWithCCF: Long = 0,
                                 unusedAAA: Long = 0,
                                 unusedMPAA: Long = 0,
                                 moneyPurchaseAA: Long = 0,
                                 alternativeAA: Long = 0,
                                 dbist: Long = 0,
                                 mpist: Long = 0,
                                 alternativeChargableAmount: Long = 0,
                                 defaultChargableAmount: Long = 0,
                                 cumulativeMP: Long = 0,
                                 cumulativeDB: Long = 0,
                                 exceedingMPAA: Long = 0,
                                 exceedingAAA: Long = 0,
                                 preFlexiSavings: Long = 0,
                                 postFlexiSavings: Long = 0,
                                 isMPA: Boolean = false,
                                 acaCF: Long = 0,
                                 dcaCF: Long = 0) extends Summary

/**
 Summary object providing read/write for JSON values. JSON is marshalled/unmarshalled from generic interface 
 not the case class.
 */
object Summary {
  implicit val summaryResultWrites: Writes[Summary] = (
    (JsPath \ "chargableAmount").write[Long] and
    (JsPath \ "exceedingAAAmount").write[Long] and 
    (JsPath \ "availableAllowance").write[Long] and
    (JsPath \ "unusedAllowance").write[Long] and 
    (JsPath \ "availableAAWithCF").write[Long] and
    (JsPath \ "availableAAWithCCF").write[Long] and
    (JsPath \ "unusedAAA").write[Long] and
    (JsPath \ "unusedMPAA").write[Long] and 
    (JsPath \ "exceedingMPAA").write[Long] and 
    (JsPath \ "exceedingAAA").write[Long] and
    (JsPath \ "isMPA").write[Boolean] and 
    (JsPath \ "moneyPurchaseAA").write[Long] and
    (JsPath \ "alternativeAA").write[Long]
  )(Summary.unapply _ )

  implicit val summaryResultReads: Reads[Summary] = (
    (JsPath \ "chargableAmount").read[Long] and
    (JsPath \ "exceedingAAAmount").read[Long] and
    (JsPath \ "availableAllowance").read[Long] and
    (JsPath \ "unusedAllowance").read[Long] and
    (JsPath \ "availableAAWithCF").read[Long] and
    (JsPath \ "availableAAWithCCF").read[Long] and
    (JsPath \ "unusedAAA").read[Long] and
    (JsPath \ "unusedMPAA").read[Long] and
    (JsPath \ "exceedingMPAA").read[Long] and 
    (JsPath \ "exceedingAAA").read[Long] and
    (JsPath \ "isMPA").read[Boolean] and
    (JsPath \ "moneyPurchaseAA").read[Long] and
    (JsPath \ "alternativeAA").read[Long]
  )(Summary.apply _)

  def unapply(summary: Summary): (Long, Long, Long, Long, Long, Long, Long, Long, Long, Long, Boolean, Long, Long) = {
    (summary.chargableAmount, summary.exceedingAAAmount, summary.availableAllowance, summary.unusedAllowance, summary.availableAAWithCF, summary.availableAAWithCCF, summary.unusedAAA, summary.unusedMPAA, summary.exceedingMPAA, summary.exceedingAAA, summary.isMPA, summary.moneyPurchaseAA, summary.alternativeAA)
  }

  def apply(chargableAmount: Long = 0,
                exceedingAAAmount: Long = 0,
                availableAllowance: Long = 0,
                unusedAllowance: Long = 0,
                availableAAWithCF: Long = 0,    // total available allowance for current year should be renamed to totalAA
                availableAAWithCCF: Long = 0,   // available allowance carried forward to following year
                unusedAAA: Long = 0,
                unusedMPAA: Long = 0,
                exceedingMPAA: Long = 0,
                exceedingAAA: Long = 0,
                isMPA: Boolean = false,
                moneyPurchaseAA: Long = 0, 
                alternativeAA: Long = 0): Summary = {
    SummaryResult(chargableAmount,
                  exceedingAAAmount,
                  availableAllowance,
                  unusedAllowance,
                  availableAAWithCF,
                  availableAAWithCCF,
                  unusedAAA,
                  unusedMPAA,
                  exceedingMPAA,
                  exceedingAAA,
                  isMPA,
                  moneyPurchaseAA,
                  alternativeAA)
  }
}

/** 
 TaxYearResults providing read/write for JSON and implicit casts.
 */
object TaxYearResults {
  import calculators.Utilities._

  implicit val summaryWrites: Writes[TaxYearResults] = (
    (JsPath \ "input").write[Contribution] and
    (JsPath \ "summaryResult").write[Summary] 
  )(unlift(TaxYearResults.unapply))

  implicit val summaryReads: Reads[TaxYearResults] = (
    (JsPath \ "input").read[Contribution] and
    (JsPath \ "summaryResult").read[Summary]
  )(TaxYearResults.apply _)

  /**
    Implicit cast function from TaxYearResults to SummaryResultsTuple.
    Used when calculating actual unused allowance.
  */
  implicit def convert(result: TaxYearResults): SummaryResultsTuple = {
    result match {
      case TaxYearResults(input, summary) => (result.input.taxPeriodStart.year, summary.exceedingAAAmount, summary.unusedAllowance)
    }
  }

  /**
    Implicit cast function from sequence of TaxYearResults to list of SummaryResultsTuple.
    Used when calculating acutal unused allowance.
  */
  implicit def convert(p:Seq[TaxYearResults]): List[SummaryResultsTuple] = p map { a => a: SummaryResultsTuple } toList
}
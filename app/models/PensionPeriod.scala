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
  Simple date object representing a point in time within a pension period.
*/
case class PensionPeriod(year: Int, month: Int, day: Int) {
  require(year > 0 && month > 0 && month < 13 && day > 0 && day < 32)
  def <(that: PensionPeriod): Boolean = 
    if (year == that.year && month == that.month && day == that.day) 
      false 
    else 
      year < that.year || (year == that.year && month < that.month) || (year == that.year && month == that.month && day < that.day) 

  def >(that: PensionPeriod): Boolean = 
    if (year == that.year && month == that.month && day == that.day) 
      false 
    else 
      year > that.year || (year == that.year && month > that.month) || (year == that.year && month == that.month && day > that.day) 

  def <=(that: PensionPeriod): Boolean = 
    if (year == that.year && month == that.month && day == that.day) 
      true 
    else 
      this < that

  def >=(that: PensionPeriod): Boolean = 
    if (year == that.year && month == that.month && day == that.day) 
      true 
    else 
      this > that

  def isPeriod(s:PensionPeriod, e:PensionPeriod):Boolean =
    (this >= s) && (this <= e)

  def isPeriod1(): Boolean =
    isPeriod(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END)

  def isPeriod2(): Boolean =
    isPeriod(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END)

  def taxYear(): Int =
    if (this < PensionPeriod(year, 4, 6) && this > PensionPeriod(year-1, 4, 6)) 
      year-1
    else if (this > PensionPeriod(year, 4, 6) && this < PensionPeriod(year+1, 4, 6)) 
      year
    else 
      year
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

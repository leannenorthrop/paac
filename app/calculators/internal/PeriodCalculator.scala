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

package calculators.internal

import models._
import calculators.internal.utilities._

trait PeriodCalculator extends ExtendedSummaryCalculator with DetailsCalculator {
  def previous3YearsUnusedAllowance(implicit previous:Seq[TaxYearResults]): Long = {
    val previousPeriods = previous.filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)
    previousPeriods.headOption.map {
      (row)=>
      implicit val contribution = Contribution(2015, Some(InputAmounts(0L,0L)))
      // use simple basic extractor since period 1 and 2 are removed above and only dealing with years prior to 2015
      val pp = previousPeriods.dropWhile(_._1 == 2015)
      val lst = actualUnusedList(PeriodCalculator(allowance))(pp, contribution).dropWhile(_._1 == 2015).slice(0,3)

      val desc = lst match {
        case head :: Nil => s"unused_${head._1}:${fmt(head._2)};"
        case head :: tail => tail.foldLeft(s"unused_${head._1}:${fmt(head._2)};")((str,pair)=>str + s"op:+;unused_${pair._1}:${fmt(pair._2)};")
        case _ => ""
      }
      detail("allowance.unused3y.calculation",desc)

      lst.foldLeft(0L)(_ + _._2)
    }.getOrElse(0L)
  }
}

object PeriodCalculator {
  def apply(allowanceInPounds: Long)(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): PeriodCalculator = {
    implicit val annualAllowanceInPounds = allowanceInPounds
    contribution match {
      case c if c.isPeriod1 => Period1Calculator()
      case _ => Period2Calculator()
    }
  }
}

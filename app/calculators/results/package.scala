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

package calculators.results

import models._
import calculators._
import calculators.Utilities._
import calculators.periods.PeriodCalculator

package object Utilities {
  /**
    Simple extractor to convert list of tax year results and 'current' contribution into a simplified tuple list in forward order (e.g. 2008, 2009, 2010)
    for results up to but not including 2015.
  */
  val extractor: SummaryCalculator => ToTupleFn = calc => {
    (p, c) => {
      implicit val previousPeriods = p
      implicit val contribution = c
      implicit val calculator = PeriodCalculator(calc.allowance)
      (List[SummaryResultsTuple](c) ++ List[SummaryResultsTuple](previousPeriods:_*)).reverse
    }
  }

  /**
    Simple form of helper function to calculate actual unused from 'current' contribution and previous results.
    This is here because the summary results only ever hold the unused allowance for the particular year in question
    but subsequent years can reduce the unused allowance further by exceeding the 'annual' allowance
  */
  def basicActualUnused(calculator: SummaryCalculator): Int => (Seq[TaxYearResults], Contribution) => Long = years => (p,c) => actualUnused(extractor(calculator))(years)(p,c)

  def sumActualUnused(calculator: SummaryCalculator): Int => (Seq[TaxYearResults], Contribution) => Long = years => (p,c) => actualUnused(extractorFn(calculator))(years)(p,c)

  def actualUnusedValues(calculator: SummaryCalculator): (Seq[TaxYearResults], Contribution) => List[YearActualUnusedPair] = (p,c) => actualUnusedAllowancesFn(extractorFn(calculator))(p,c)

  def excludePreTrigger(p:Seq[TaxYearResults]): Seq[TaxYearResults] = {
    val list = p.reverse
    list.find(_.input.isTriggered).map {
      (firstTriggered) =>
      val index = list.indexOf(firstTriggered)
      if (index > 0) list.filterNot(_ == list(index-1)).reverse else p
    }.getOrElse(p)
  }

  /**
  * Extractor to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
  * taking into consideration 2015 periods 1 and 2
  */
  val extractorFn: SummaryCalculator => ToTupleFn = calc => (p,c) => {
    implicit val previousPeriods = p
    implicit val contribution = c
    implicit val calculator = calc
    
    def fetch(m: Map[Boolean, Seq[TaxYearResults]], key: Boolean): List[TaxYearResults] = m.getOrElse(key, List[TaxYearResults]()).toList

    // Remove pre-trigger row from results to ensure we don't get duplicate year and group
    val pre2015Map = excludePreTrigger(previousPeriods).groupBy(_.input.taxPeriodStart.taxYear < 2015)
    val y2015Map = pre2015Map(false).groupBy(_.input.taxPeriodStart.taxYear == 2015)

    // Pre-2015 values prior to deducting 2015 period 1 exceeding amount
    val originalPre2015: List[SummaryResultsTuple] = fetch(pre2015Map, true)

    // If period 1 is present then deduct period 1 exceeding from unused in pre-2015 values
    val maybePeriod1 = fetch(y2015Map, true).find((c)=>c.input.isPeriod1)
    val maybePeriod2 = fetch(y2015Map, true).find((c)=>c.input.isPeriod2)
    val pre2015 = maybePeriod1.map {
      (period1)=>
      if (period1.summaryResult.exceedingAAAmount > 0)
        calculators.periods.Utilities.deductPeriod1Exceeding((originalPre2015).reverse, period1.summaryResult).reverse
      else
        originalPre2015
    }.getOrElse(originalPre2015)
    // Now that period 1 has been deducted if necessary create year 2015 from period 2 results
    val y2015: List[SummaryResultsTuple] = maybePeriod2.map(TaxYearResults.convert(_)).toList

    // Values after 2015
    val post2015: List[SummaryResultsTuple] = fetch(y2015Map, false)

    // 'This' year
    val currentYear: List[SummaryResultsTuple] = List[SummaryResultsTuple](contribution)

    (currentYear ++ post2015 ++ y2015 ++ pre2015).reverse
  }
}

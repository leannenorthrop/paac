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

package calculators.periods

import models._
import calculators.results.Utilities._
import calculators.Utilities._

package object Utilities {
  def isPeriod1(taxYearResult: TaxYearResults): Boolean = taxYearResult.input.isPeriod1 
  def isPeriod2(taxYearResult: TaxYearResults): Boolean = taxYearResult.input.isPeriod2
  def isBefore2015(taxYearResult: TaxYearResults): Boolean = every(complement(any(isPeriod1,isPeriod2)), beforeYear(2015))(taxYearResult)
  def isTaxResultNotTriggered(tx: TaxYearResults): Boolean = complement(isTaxResultTriggered)(tx)
  def isTaxResultTriggered(tx: TaxYearResults): Boolean = every(any(isPeriod1,isPeriod2), isTriggered)(tx)

  def notTriggered(implicit previousPeriods:Seq[TaxYearResults]): Option[TaxYearResults] = previousPeriods.find(isTaxResultNotTriggered)
  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = notTriggered.flatMap(maybeExtended(_))
  def preTriggerInputs(implicit previousPeriods:Seq[TaxYearResults]): Option[Contribution] = notTriggered.map(_.input)

  def deductPeriod1Exceeding(list: List[SummaryResultsTuple], sr: Summary): List[SummaryResultsTuple] = {
    // get pre 2015 results
    val pre2015Results = list.filter { case(year,_,_) => year < 2015 }.reverse

    // deduct exceeding amount from previously unused allowances giving new list of unused allowances
    // dropping the period 1 2015 result from the list
    val current: SummaryResultsTuple = (2015, 0, sr.unusedAllowance)
    val newUnusedAllowances = useAllowances(sr.exceedingAAAmount, current::pre2015Results).drop(1).reverse

    // splice in new list of unused allowances to build new complete list of unused allowances
    val (before,after) = pre2015Results.reverse.splitAt(4)
    val newAfter = newUnusedAllowances.zip(after).map { case ((_,actualUnused), (year,exceeding,_)) => (year, exceeding, actualUnused) }
    before ++ newAfter ++ list.filter { case(year,_,_) => year > 2014 }
  }

  /**
  * Helper method to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
  * taking into consideration if the contribution is period 1 or 2
  */
  val periodExtractor: PeriodCalculator => ToTupleFn = calc => (p,c) => {
    implicit val calculator = calc
    // handle period 1 and 2 separately so filter out of previous results
    val previousPeriods: Seq[TaxYearResults] = p.filterNot(_.input.isTriggered).filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)
    // add back in either period 1 *or* 2 as the result for 2015 at head of list (this method shouldn't be called for years after 2015)
    val list: List[SummaryResultsTuple] = (List[SummaryResultsTuple](c) ++ List[SummaryResultsTuple](previousPeriods:_*)).reverse
    p.find(_.input.isPeriod1) match {
      case Some(period1) => period1 match {
        case TaxYearResults(_, sr) if period1.summaryResult.exceedingAAAmount > 0 => deductPeriod1Exceeding(list, sr)
        case _ => list
      }
      case _ => list
    }
  }
}

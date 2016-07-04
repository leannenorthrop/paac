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

package object Utilities {
  def isBefore2015(taxYearResult: TaxYearResults): Boolean = !(taxYearResult.input.isPeriod1 || taxYearResult.input.isPeriod2) && taxYearResult.input.taxPeriodStart.year <= 2015
    
  def isTriggered(implicit contribution: Contribution): Boolean = contribution.isTriggered

  def taxResultNotTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !tx.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)

  def taxResultTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !taxResultNotTriggered(tx)

  def maybeExtended(t: TaxYearResults): Option[ExtendedSummaryFields] = if (t.summaryResult.isInstanceOf[ExtendedSummaryFields]) Some(t.summaryResult.asInstanceOf[ExtendedSummaryFields]) else None

  def notTriggered(implicit previousPeriods:Seq[TaxYearResults]): Option[TaxYearResults] = previousPeriods.find(taxResultNotTriggered)

  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = notTriggered.flatMap(maybeExtended(_))

  def preTriggerInputs(implicit previousPeriods:Seq[TaxYearResults]): Option[Contribution] = notTriggered.map(_.input)

  def actualUnusedAllowance(list: List[(Int,Long)])(noOfYears: Int): Long = list.slice(0,noOfYears).foldLeft(0L)(_+_._2)

  /**
  * Helper method to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
  * taking into consideration if the contribution is period 1 or 2
  */
  def extractValues(calc:PeriodCalculator)(p:Seq[TaxYearResults], c: Contribution): List[SummaryResultsTuple] = {
    implicit val contribution = c
    implicit val calculator = calc

    // handle period 1 and 2 separately so filter out of previous results
    val previousPeriods: Seq[TaxYearResults] = p.filterNot(_.input.isTriggered).filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)

    // add back in either period 1 or 2 as the result for 2015
    val list: List[SummaryResultsTuple] = (List[SummaryResultsTuple](contribution) ++ List[SummaryResultsTuple](previousPeriods:_*)).reverse

    // does period 1 exist?
    p.find(_.input.isPeriod1) match {
      case Some(period1) => period1 match {
        // yes, did period 1 exceed allowance?
        case TaxYearResults(_, sr) if period1.summaryResult.exceedingAAAmount > 0 => {
          // yes, get pre 2015 results
          val pre2015Results = list.filter(_._1<2015).reverse

          // deduct exceeding amount from previously unused allowances giving new list of unused allowances
          // dropping the 2015 result from the list
          val current: SummaryResultsTuple = (2015, 0, 0, 0, sr.unusedAllowance)
          val newUnusedAllowances = useAllowances(sr.exceedingAAAmount, current::pre2015Results).drop(1).reverse

          // splice in new list of unused allowances to build new complete list of unused allowances
          val (before,after) = pre2015Results.reverse.splitAt(4)
          val newAfter = newUnusedAllowances.zip(after).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
          before ++ newAfter ++ list.filter(_._1>2014)
        }
        // no, simply return basic list
        case _ => list
      }
      // no, simply return basic list
      case _ => list
    }
  }
}

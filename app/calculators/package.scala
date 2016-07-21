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

package calculators

import models._

package object Utilities {
  type SizeConstraint = Int => Boolean
  type ResultsFilter = TaxYearResults => Boolean
  type ToTupleFn = (Seq[TaxYearResults], Contribution) => List[SummaryResultsTuple]
  type SummaryResultsTuple = (Int, Long, Long)
  type YearActualUnusedPair = (Int,Long)

  def complement[A](predicate: A => Boolean) = (a: A) => !predicate(a)
  def any[A](predicates: (A => Boolean)*): A => Boolean = a => predicates.exists(pred => pred(a))
  def none[A](predicates: (A => Boolean)*) = complement(any(predicates: _*))
  def every[A](predicates: (A => Boolean)*) = none(predicates.view.map(complement(_)): _*)

  val yearConstraint: SizeConstraint => ResultsFilter = constraint => taxYearResult => constraint(taxYearResult.input.taxPeriodStart.taxYear)
  val afterYear: Int => ResultsFilter = year => yearConstraint(_ >= year)
  val beforeYear: Int => ResultsFilter = year => yearConstraint(_ <= year)
  val isYear: Int => ResultsFilter = year => yearConstraint(_ == year)

  def isTriggered(implicit contribution: Contribution): Boolean = contribution.isTriggered
  def isTriggered(taxYearResult: TaxYearResults): Boolean = isTriggered(taxYearResult.input)

  def maybeExtended(t: TaxYearResults): Option[ExtendedSummaryFields] = if (t.summaryResult.isInstanceOf[ExtendedSummaryFields]) Some(t.summaryResult.asInstanceOf[ExtendedSummaryFields]) else None
  
  /**
  * Use allowances by building new list of 4 years of unused allowances (current year + previous 3 years) for the current year
  * after deducting the exceeding ammount from the allowance list passed in.
  */
  def useAllowances(execeeding: Long, unusedAllowances: List[SummaryResultsTuple]): List[YearActualUnusedPair] = {
    val (thisYear,_,thisYearUnused) = unusedAllowances.head
    val allowances = unusedAllowances.slice(0,4).map { case(year, _, unusedAllowance) => (year,unusedAllowance) }
    
    // walk through 'this' year and previous 3 years unused allowances, deducting 
    // exceeding allowance, and creating new list of *actual* unused allowances
    allowances.reverse.foldLeft((execeeding,List[YearActualUnusedPair]())) {
      (pair,allowanceTuple)=>
      val (currentExceeding, yearUnusedTupleLst) = pair
      allowanceTuple match {
        // have we reached the end of the allowances list? 
        //   yes, so add this year and year's unsed allowance to end of lst
        case (year, _) if year == thisYear => (currentExceeding, (year, thisYearUnused) :: yearUnusedTupleLst)
        //   no, so have we reached exceeding of 0?
        //     yes, so simply copy tuple into list
        case (year, _) if currentExceeding <= 0 => (currentExceeding, allowanceTuple :: yearUnusedTupleLst)
        //     no. So we still have an exceeding amount therefore need to calculate new unused allowance
        case (year, unusedAllowance) => {
          // calculate new exceeding by deducting unused allowance for the year
          val ex = currentExceeding - unusedAllowance
          // is new exceeding now 0?
          if (ex < 0) {
            // yes, then is it before 2011?
            if (thisYear < 2011 && year < 2011) {
              // yes, then allowed unlimited use of allowances so simply copy allowance into list
              (ex, (year,unusedAllowance) :: yearUnusedTupleLst)
            } else {
              // no, then put new unused allowance for the year
              (ex, (year,ex.abs) :: yearUnusedTupleLst)
            }
          } else {
            // no, so 'use up' unused allowance setting it to 0
            (ex, (year,0L) :: yearUnusedTupleLst)
          }
        }
      }
    }._2
  }

  def actualUnusedList(calculator: SummaryCalculator): (Seq[TaxYearResults], Contribution) => List[YearActualUnusedPair] = 
    (p,c) => 
    actualUnusedAllowancesFn(extractorFn(calculator))(p,c)

  def actualUnused(extract: ToTupleFn)(years: Int): (Seq[TaxYearResults], Contribution) => Long =
    (p,c) => 
    actualUnusedAllowancesFn(extract)(p,c).slice(0,years).foldLeft(0L)(_+_._2)

  def actualUnused(calculator: SummaryCalculator)(years: Int): (Seq[TaxYearResults], Contribution) => Long =
    (p,c) => 
    actualUnused(extractorFn(calculator))(years)(p,c)

  protected val actualUnusedAllowancesFn: ToTupleFn => (Seq[TaxYearResults], Contribution) => List[YearActualUnusedPair]= {
    def calculate(values:List[SummaryResultsTuple]): List[SummaryResultsTuple] = {
      // walk list of year/exceeding/unused allowance building list of actual unused allowance for each year
      values.foldLeft(List[SummaryResultsTuple]()) {
        (lst,tuple)=>
        tuple match {
          case (_,execeeding,_) if execeeding < 0 => tuple :: lst
          case (_,execeeding,_) => {
            // Re-calculate unused allowances 
            // deducting the exceeding from 3rd year ago, then 2nd year ago, then a year ago as appropriate
            val unusedAllowances = tuple :: lst
            val newUnusedAllowances = useAllowances(execeeding, unusedAllowances)

            // rebuild the actual unused allowance list based on new unused allowances
            val (before,after) = unusedAllowances.splitAt(4) // todo what to do if no previous results?
            val newBefore = newUnusedAllowances.zip(before).map { case ((_,unused), (year,exceeding,_)) => (year, exceeding, unused) }
            newBefore ++ after
          }
        }
      }
    }
    
    extract =>
      (p,c) => 
        calculate(extract(p,c)) map { case (year, _, actualUnused) => (year, actualUnused) }
  }

  /**
  * Extractor to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
  * taking into consideration 2015 periods 1 and 2
  */
  protected val extractorFn: SummaryCalculator => ToTupleFn = calc => (p,c) => {
    import calculators.periods.Utilities._
    implicit val previousPeriods = p
    implicit val contribution = c
    implicit val calculator = calc
    
    groupedPreTriggerExcluded(p).map {
      (entry) =>
      entry._1 match {
        case "<2015" => {
          // because we drop 2015 period 1 we need to 
          // deduct any period 1 exceeding amounts from the pre-2015 list
          // if it exists
          groupedPreTriggerExcluded(p).get("2015").map {
            (year2015)=>
            year2015.find((c) => c.input.isPeriod1 && c.summaryResult.exceedingAAAmount > 0).map {
              (period1) =>
              deductPeriod1Exceeding(List[SummaryResultsTuple](entry._2: _*).reverse, period1.summaryResult).reverse
            }.getOrElse(List[SummaryResultsTuple](entry._2: _*))
          }.getOrElse(List[SummaryResultsTuple](entry._2: _*))
        }
        case "2015" => entry._2.find(_.input.isPeriod2).map(TaxYearResults.convert(_)).toList
        case _ => List[SummaryResultsTuple](entry._2: _*)
      }
    }.flatten.toList.sortBy(_._1) ++ List[SummaryResultsTuple](contribution)
  }
  
  protected def excludePreTrigger(p:Seq[TaxYearResults]): Seq[TaxYearResults] = {
    val list = p.reverse
    list.find(_.input.isTriggered).map {
      (firstTriggered) =>
      val index = list.indexOf(firstTriggered)
      if (index > 0) list.filterNot(_ == list(index-1)).reverse else p
    }.getOrElse(p)
  }

  def groupedPreTriggerExcluded(p: Seq[TaxYearResults]): Map[String, Seq[TaxYearResults]] = grouped(excludePreTrigger(p))

  def grouped(p: Seq[TaxYearResults]): Map[String, Seq[TaxYearResults]] = p.groupBy {
      (c)=>
      c.input.taxPeriodStart.taxYear match {
        case year if year < 2015 => "<2015"
        case year if year == 2015 => "2015"
        case year if year > 2015 => ">2015"
      }
    }
}

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

package object Utilities {
  type SummaryResultsTuple = (Int, Long, Long)

  type YearActualUnusedPair= (Int,Long)

  type ToTupleFn = (Seq[TaxYearResults], Contribution) => List[SummaryResultsTuple]

  /**
  * Helper method to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010)
  */
  def toSummaryResultsTuple(b: BasicCalculator)(p:Seq[TaxYearResults], c: Contribution): List[SummaryResultsTuple] = {
    implicit val calculator = calculators.periods.PeriodCalculator(b.annualAllowanceInPounds)(p,c).get
    (List[SummaryResultsTuple](c) ++ List[SummaryResultsTuple](p:_*)).reverse
  }

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

  def calculateActualUnused(extract: ToTupleFn)(previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[YearActualUnusedPair] = {
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
            val (before,after) = unusedAllowances.splitAt(4)
            val newBefore = newUnusedAllowances.zip(before).map { case ((_,unused), (year,exceeding,_)) => (year, exceeding, unused) }
            newBefore ++ after
          }
        }
      }
    }

    // convert results into tuple and calculate actual unused allowances
    calculate(extract(previousPeriods, contribution)).map { case (year, _, actualUnused) => (year, actualUnused) }
  }
}

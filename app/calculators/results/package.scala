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
  type SummaryResultsTuple = (Int, Long, Long, Long, Long)

  type ActualUnusedAllowanceTuple = (Int,Long)

  type ToTupleFn = (Seq[TaxYearResults], Contribution) => List[SummaryResultsTuple]

  /**
  * Helper method to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010)
  */
  def toSummaryResultsTuple(b: BasicCalculator)(previousPeriods:Seq[TaxYearResults], c: Contribution): List[SummaryResultsTuple] = {
    implicit val contribution = c
    (List((contribution.taxPeriodStart.year, b.definedBenefit, b.annualAllowance, b.exceedingAllowance, b.unusedAllowance)) ++ 
    previousPeriods.map {
      (result) =>
      val amounts = result.input.amounts.getOrElse(InputAmounts())
      val summary = result.summaryResult
      (result.input.taxPeriodStart.year, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
    }.toList).reverse
  }

  /**
  * Helper function to build new list of unused allowances after deducting the exceeding ammount from the allowance list passed in.
  */
  def useAllowances(execeeding: Long, thisYear: Int, thisYearAllowance: Long, thisYearUnused: Long, lst: List[SummaryResultsTuple]): List[ActualUnusedAllowanceTuple] = {
    val previousYearsAllowances = lst.slice(0,3).map((t)=>(t._1,t._5))
    val allowances = (thisYear, thisYearAllowance) :: previousYearsAllowances
    
    var i = 0
    val l = allowances.reverse.foldLeft((execeeding,List[(Int,Long)]())) {
      (pair,allowanceTuple)=>
      i = i + 1
      val currentExceeding = pair._1
      if (allowanceTuple._1 == thisYear) {
        (0, (allowanceTuple._1, thisYearUnused) :: pair._2)
      } else if (currentExceeding <= 0) {
        (0, allowanceTuple :: pair._2)
      } else {
        val ex = currentExceeding - allowanceTuple._2
        if (ex < 0) {
          if (thisYear < 2011 && allowanceTuple._1 < 2011) {
            (ex, (allowanceTuple._1,allowanceTuple._2) :: pair._2)
          } else {
            (ex, (allowanceTuple._1,ex.abs) :: pair._2)
          }
        } else {
          (ex, (allowanceTuple._1,0L) :: pair._2)
        }
      }
    }._2
    l
  }

  def calculateActualUnused(extract: ToTupleFn)(previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[ActualUnusedAllowanceTuple] = {
    def calculate(values:List[SummaryResultsTuple]): List[SummaryResultsTuple] = {
      // walk results building list of actual unused allowance
      values.foldLeft(List[SummaryResultsTuple]()) {
        (lst,tuple)=>
        val execeeding = tuple._4
        // Did the year exceed the allowance?
        if (execeeding < 0) {
          // no - simply copy tuple into result
          tuple :: lst
        } else {
          // yes - so re-calculate unused allowances 
          // deducting the exceeding from 3rd year ago, then 2nd year ago, then a year ago as appropriate
          val newUnusedAllowances = useAllowances(execeeding, tuple._1, tuple._3, tuple._5, lst)

          // rebuild the result list based on new unused allowances
          val (before,after) = (tuple :: lst).splitAt(4)
          val newBefore = newUnusedAllowances.zip(before).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
          newBefore ++ after
        }
      }
    }

    // convert results into tuple and calculate actual unused allowances
    calculate(extract(previousPeriods, contribution)).map((tuple)=>(tuple._1, tuple._5))
  }
}

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

case class BasicCalculator(annualAllowanceInPounds: Long) extends calculators.Calculator {
  calc => BasicCalculator

  def definedBenefit(implicit contribution: Contribution): Long = {
    val amounts = contribution.amounts.getOrElse(InputAmounts())
    val year = contribution.taxPeriodStart.year
    if (year < 2015)
      amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
    else {
      if (year == 2015 && !contribution.isTriggered) {
        amounts.definedBenefit.getOrElse(0L) + amounts.moneyPurchase.getOrElse(0L)
      } else {
        amounts.definedBenefit.getOrElse(0L)
      }
    }
  }

  def definedContribution(implicit contribution: Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def annualAllowance(): Long = annualAllowanceInPounds*100L // convert allowance from pounds to pence

  def exceedingAllowance(implicit contribution: Contribution): Long = (calc.definedBenefit - calc.annualAllowance).max(0)

  def unusedAllowance(implicit contribution: Contribution): Long = (calc.annualAllowance - calc.definedBenefit).max(0)

  // total annual allowance possible
  // LN TODO Update to consider 2015 2 periods if this is reused for 2016
  def annualAllowanceCF(implicit previousPeriods:Seq[TaxYearResults]): Long = {
    previousPeriods.map(_.summaryResult.availableAAWithCCF).headOption.map(_ + calc.annualAllowance).getOrElse(calc.annualAllowance)
  }

  // cumulative carry forwards is 2 previous years plus current year's annual allowance - used allowance
  def annualAllowanceCCF(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (contribution.taxPeriodStart.year < 2011) {
      // Prior to 2011 nothing was liable for tax charge and carry forwards are allowed
      //println("*** " + actualUnused.slice(0,3).mkString(","))
      val unusedAllowanceList = actualUnused.slice(0,3).map(_._2)
      unusedAllowanceList.foldLeft(0L)(_+_)
    } else {
      val execeeding = calc.exceedingAllowance
      if (execeeding > 0) {
        val previousResults = previousPeriods.map(_.summaryResult).headOption.getOrElse(SummaryResult())
        if (execeeding >= previousResults.availableAAWithCCF){
          0L
        } else {
          val unusedAllowanceList = actualUnused.slice(0,3).map(_._2)
          unusedAllowanceList.foldLeft(0L)(_+_)
        }
      } else {
        val unusedAllowanceList = actualUnused.slice(0,3).map(_._2)
        unusedAllowanceList.foldLeft(0L)(_+_)
      }
    }
  }

  def chargableAmount(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Long = {
    if (contribution.taxPeriodStart.year < 2011) -1 else {
      (calc.definedBenefit - calc.annualAllowanceCF).max(0)
    }
  }

  def actualUnused(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[(Int,Long)] = {
    type FlatValues = (Int, Long, Long, Long, Long)
    def extractFlatValues(): List[FlatValues] = {
      (List((contribution.taxPeriodStart.year, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance)) ++ 
      previousPeriods.map {
        (result) =>
        val amounts = result.input.amounts.getOrElse(InputAmounts())
        val summary = result.summaryResult
        (result.input.taxPeriodStart.year, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
      }.toList).reverse
    }
    def useAllowances(execeeding: Long, thisYear: Int, thisYearAllowance: Long, lst: List[FlatValues]): List[(Int,Long)] = {
      val previousYearsAllowances = lst.slice(0,3).map((t)=>(t._1,t._5))
      val allowances = (thisYear, thisYearAllowance) :: previousYearsAllowances
      //println(" - " + allowances.mkString(","))
      val l = allowances.reverse.foldLeft((execeeding+thisYearAllowance,List[(Int,Long)]())) {
        (pair,allowanceTuple)=>
        val currentExceeding = pair._1
        if (currentExceeding <= 0) {
          (0, allowanceTuple :: pair._2)
        } else {
          val ex = currentExceeding - allowanceTuple._2
          if (ex < 0) {
            (ex, (allowanceTuple._1,ex.abs) :: pair._2)
          } else {
            (ex, (allowanceTuple._1,0L) :: pair._2)
          }
        }
      }._2
      //println(" * " + thisYear + " " + l.mkString(","))
      l
    }
    def calculate(values:List[FlatValues]): List[FlatValues] = {
      values.foldLeft(List[FlatValues]()) {
        (lst,tuple)=>
        val execeeding = tuple._4
        if (execeeding <= 0) {
          tuple :: lst
        } else {
          val newUnusedAllowances = useAllowances(execeeding, tuple._1, tuple._3, lst)
          val (before,after) = (tuple :: lst).splitAt(4)
          val newBefore = newUnusedAllowances.zip(before).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
          newBefore ++ after
        }
      }
    }
    calculate(extractFlatValues()).map((tuple)=>(tuple._1, tuple._5))
  }

  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    contribution.amounts.map {
      _ =>
      SummaryResult(calc.chargableAmount, 
                    calc.exceedingAllowance, 
                    calc.annualAllowance, 
                    calc.unusedAllowance, 
                    calc.annualAllowanceCF, 
                    calc.annualAllowanceCCF)
    }
  }
}

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

trait PeriodCalculator {
  def definedContribution(implicit contribution:Contribution): Long = contribution.amounts.getOrElse(InputAmounts()).moneyPurchase.getOrElse(0L)

  def basicCalculator(): calculators.results.BasicCalculator
  def definedBenefit(): Long
  def chargableAmount(): Long
  def exceedingAllowance(): Long
  def annualAllowance(): Long
  def unusedAllowance(): Long
  def aaCF(): Long
  def aaCCF(): Long
  def moneyPurchaseAA(): Long = 0L
  def alternativeAA(): Long = 0L
  def dbist(): Long = 0L
  def mpist(): Long = 0L
  def alternativeChargableAmount(): Long = 0L
  def defaultChargableAmount(): Long = 0L
  def cumulativeMP(): Long = 0L
  def cumulativeDB(): Long = 0L
  def exceedingMPAA(): Long = 0L
  def exceedingAAA(): Long = 0L
  def unusedAAA(): Long = 0L
  def unusedMPAA(): Long = 0L
  def preFlexiSavings(): Long = 0L
  def postFlexiSavings(): Long = 0L
  def isMPAAApplicable(): Boolean = false
  def acaCF() : Long = 0L
  def dcaCF() : Long = 0L

  def isTriggered(implicit contribution: Contribution): Boolean = contribution.isTriggered

  def taxResultNotTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !tx.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)

  def taxResultTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !taxResultNotTriggered(tx)

  def previousInputs(implicit previousPeriods:Seq[TaxYearResults]): InputAmounts = previousPeriods.headOption.map(_.input.amounts.getOrElse(InputAmounts())).getOrElse(InputAmounts())

  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = previousPeriods.find(taxResultNotTriggered).map(_.summaryResult.asInstanceOf[ExtendedSummaryFields])

  def preTriggerAmounts(implicit previousPeriods:Seq[TaxYearResults]): Option[InputAmounts] = previousPeriods.find(taxResultNotTriggered).flatMap(_.input.amounts)

  def previous3YearsUnusedAllowance()(implicit previousPeriods:Seq[TaxYearResults], c: Contribution): Long = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val contribution = Contribution(c.taxPeriodStart, c.taxPeriodEnd, Some(InputAmounts(0L,0L)))

    val l = if (!previousPeriods.find(_.input.isPeriod1).isDefined) {
      basicCalculator().actualUnused(previousPeriods, contribution).drop(1).slice(0,3)
    } else {
      basicCalculator().actualUnused(previousPeriods.drop(1), contribution).drop(1).slice(0,3)
    }
    
    l.foldLeft(0L)(_+_._2)
  }

  def actualUnused(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): List[(Int,Long)] = {
    type FlatValues = (Int, Long, Long, Long, Long)
    def extractFlatValues(implicit p:Seq[TaxYearResults], contribution: Contribution): List[FlatValues] = {
      val previousPeriods = p.filterNot(_.input.isTriggered)
      val prefix = if (contribution.isPeriod1()) {
        List((20151, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      } else if (contribution.isPeriod2()) {
        List((20152, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      } else {
        List((contribution.taxPeriodStart.year, definedBenefit, annualAllowance, exceedingAllowance, unusedAllowance))
      }
      val list = (prefix ++
        previousPeriods.map {
          (result) =>
            val amounts = result.input.amounts.getOrElse(InputAmounts())
            val summary = result.summaryResult
            if (result.input.isPeriod1) {
              (20151, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
            } else if (result.input.isPeriod2) {
              (20152, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
            } else {
              (result.input.taxPeriodStart.year, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
            }
        }.toList).reverse
      list
    }

    def useAllowances(execeeding: Long, thisYear: Int, thisYearAllowance: Long, thisYearUnused: Long, lst: List[FlatValues]): List[(Int,Long)] = {
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

    def calculate(values:List[FlatValues]): List[FlatValues] = {
      val mostRecentYear = values.reverse.head._1
      val isPrint = false
      values.foldLeft(List[FlatValues]()) {
        (lst,tuple)=>
          val newLst = {
            val execeeding = tuple._4
            if (execeeding < 0) {
              if (tuple._1 == 20151 && mostRecentYear == 20152) {
                lst
              } else {
                tuple :: lst
              }
            } else {
              val l = if (tuple._1 == 20152 && mostRecentYear == 20152) {
                useAllowances(execeeding, tuple._1, tuple._3, tuple._5, lst.drop(1))
              } else {
                useAllowances(execeeding, tuple._1, tuple._3, tuple._5, lst)
              }
              val (before,after) = (tuple :: lst).splitAt(4)
              val newBefore = l.zip(before).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
              (newBefore ++ after)
            }
          }
          newLst
      }
    }
    calculate(extractFlatValues).map((tuple)=>(tuple._1, tuple._5))
  }
}

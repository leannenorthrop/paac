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

package logic

import models._

trait Calculator {
  def summary(previousPeriods:Seq[SummaryResult], contribution:Contribution) : Option[SummaryResult]
}

object CalculatorFactory {
  def get(contribution:Contribution) : Option[Calculator] = contribution match {
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year < 2014 && year > 2007 => Some(Pre2014Calculator)
    /*case Contribution(TaxPeriod(year, _, _ ), _, _) if year == 2014 => Some(Year2014Calculator)
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year == 2015 => Some(Year2015Period1Calculator)
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year == 2015 => Some(Year2015Period2Calculator)
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year > 2015 => Some(Post2015Period2Calculator)*/
    case _ => None
  }
}

object Pre2014Calculator extends Calculator {
  def summary(previousPeriods:Seq[SummaryResult], contribution: models.Contribution): Option[SummaryResult] = contribution match {
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year < 2014 && year > 2007 =>
      val annualAllowance: Long = 50000*100
      val exceedingAAAmount: Long = (contribution.amounts.definedBenefit - annualAllowance).max(0)
      val unusedAllowance: Long = (annualAllowance - contribution.amounts.definedBenefit).max(0)
      val availableAAWithCF: Long = annualAllowance + previousPeriods.slice(0,3).foldLeft(0L)(_+_.unusedAllowance)
      val availableAAWithCCF: Long = (annualAllowance + previousPeriods.slice(0,2).foldLeft(0L)(_+_.unusedAllowance) - (contribution.amounts.definedBenefit-exceedingAAAmount)).max(0)
      val chargableAmount: Long = if (previousPeriods.slice(0,3).size < 3) 0 else (contribution.amounts.definedBenefit - availableAAWithCF).max(0)

      Some(SummaryResult(chargableAmount, exceedingAAAmount, annualAllowance, unusedAllowance, availableAAWithCF, availableAAWithCCF))
    case _ => None
  }
}

/*object Year2014Calculator extends Calculator {
  def summary(contribution: models.Contribution): Option[SummaryResult] = Some(SummaryResult())
}

object Year2015Period1Calculator extends Calculator {
  def summary(contribution: models.Contribution): Option[SummaryResult] = Some(SummaryResult())
}

object Year2015Period2Calculator extends Calculator {
  def summary(contribution: models.Contribution): Option[SummaryResult] = Some(SummaryResult())
}

object Post2015Period2Calculator extends Calculator {
  def summary(contribution: models.Contribution): Option[SummaryResult] = Some(SummaryResult())
}*/

trait PensionAllowanceCalculator {
  def calculateAllowances(contributions : Seq[Contribution]) : Seq[TaxYearResults] = {
    // Ensure sequential tax years
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)
    val allContributions = (inputsByTaxYear.keys.min to inputsByTaxYear.keys.max).foldLeft(List[Contribution]()) {
      (lst:List[Contribution], year:Int) =>
      // TODO implement support partial tax years
      inputsByTaxYear.get(year).getOrElse(List(Contribution(year,0))).head :: lst
    }.sortWith(_.taxPeriodStart.year < _.taxPeriodStart.year)

    // Calculate results
    allContributions.foldLeft(List[TaxYearResults]()) {
      (lst, contribution) =>

      val summary = CalculatorFactory.get(contribution).map(_.summary(lst.map(_.summaryResult), contribution).getOrElse(SummaryResult())).getOrElse(SummaryResult())
      
      TaxYearResults(contribution, summary) :: lst
    }.reverse
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

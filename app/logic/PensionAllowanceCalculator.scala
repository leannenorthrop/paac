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
  def summary(previousPeriod:SummaryResult, contribution:Contribution) : Option[SummaryResult]
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
  def summary(previousPeriod:SummaryResult, contribution: models.Contribution): Option[SummaryResult] = contribution match {
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year < 2014 && year > 2007 =>
      val availableAllowance: Long = 50000
      val chargableAmount: Long = 0
      val exceedingAAAmount: Long = if (contribution.amounts.definedBenefit > availableAllowance) contribution.amounts.definedBenefit - availableAllowance else 0
      val unusedAllowance: Long = availableAllowance - contribution.amounts.definedBenefit
      val availableAllowanceWithCF: Long = availableAllowance + previousPeriod.unusedAllowance
      val availableAllowanceWithCCF: Long = availableAllowanceWithCF - contribution.amounts.definedBenefit

      Some(SummaryResult(chargableAmount, exceedingAAAmount, availableAllowance, unusedAllowance, availableAllowanceWithCF, availableAllowanceWithCCF))
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
    contributions.sortWith(_.taxPeriodStart.year < _.taxPeriodStart.year).foldLeft(List[TaxYearResults]()) {
      (lst, contribution) =>

      val previousSummary = lst.headOption.map(_.summaryResult).getOrElse(SummaryResult())
      val summary = CalculatorFactory.get(contribution).map(_.summary(previousSummary, contribution).getOrElse(SummaryResult())).getOrElse(SummaryResult())
      
      TaxYearResults(contribution, summary) :: lst
    }.reverse
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

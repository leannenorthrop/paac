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
  def summary(contribution:Contribution) : SummaryResult
}

object CalculatorFactory {
  def get(contribution:Contribution) : Option[Calculator] = contribution match {
    case Contribution(year, _) if year < 2014 => Some(Pre2014Calculator)
    case Contribution(year, _) if year == 2014 => Some(Year2014Calculator)
    case Contribution(year, _) if year == 2015 => Some(Year2015Period1Calculator)
    case Contribution(year, _) if year == 2015 => Some(Year2015Period2Calculator)
    case Contribution(year, _) if year > 2015 => Some(Post2015Period2Calculator)
    case _ => None
  }
}

object Pre2014Calculator extends Calculator {
  def summary(contribution: models.Contribution): models.SummaryResult = SummaryResult()
}

object Year2014Calculator extends Calculator {
  def summary(contribution: models.Contribution): models.SummaryResult = SummaryResult()
}

object Year2015Period1Calculator extends Calculator {
  def summary(contribution: models.Contribution): models.SummaryResult = SummaryResult()
}

object Year2015Period2Calculator extends Calculator {
  def summary(contribution: models.Contribution): models.SummaryResult = SummaryResult()
}

object Post2015Period2Calculator extends Calculator {
  def summary(contribution: models.Contribution): models.SummaryResult = SummaryResult()
}

trait PensionAllowanceCalculator {
  def calculateAllowances(contributions : Seq[Contribution]) : Seq[TaxYearResults] = {
    contributions.map {
      contribution =>
      val summary = CalculatorFactory.get(contribution).map(_.summary(contribution)).getOrElse(SummaryResult())
      TaxYearResults(contribution, summary)
    }
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator

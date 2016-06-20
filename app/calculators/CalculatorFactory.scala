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
import calculators.results._

trait Calculator {
  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution) : Option[Summary]
}

trait AllowanceCalculator extends Calculator {
  def allowance(): Long
  def isSupported(contribution:Contribution):Boolean
}

trait CalculatorFactory {
  protected val calculators : List[AllowanceCalculator]
  def get(contribution:Contribution) : Option[AllowanceCalculator] = calculators.find(_.isSupported(contribution))
}

object CalculatorFactory extends CalculatorFactory {
  protected override val calculators : List[AllowanceCalculator] = List(Pre2014Calculator, Year2014Calculator, Year2015Period1Calculator, Year2015Period2Calculator)
}

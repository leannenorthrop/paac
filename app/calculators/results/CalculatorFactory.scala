/*
 * Copyright 2017 HM Revenue & Customs
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

trait Calculator {
  def allowance(contribution:Contribution): Long
  def summary(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Option[Summary]
}

protected trait FactoryCalculator extends Calculator {
  def isSupported(contribution:Contribution):Boolean
}

protected trait CalculatorFactory {
  protected val calculators : List[FactoryCalculator]
  protected def get(contribution:Contribution) : Option[FactoryCalculator] = calculators.find(_.isSupported(contribution))
}

object Calculator extends CalculatorFactory {
  protected override val calculators : List[FactoryCalculator] = List(Pre2014Calculator,
                                                                      Year2014Calculator,
                                                                      Year2015Period1Calculator,
                                                                      Year2015Period2Calculator,
                                                                      Post2015Calculator)

  def apply(contribution:Contribution): Calculator = {
    get(contribution).getOrElse {
      new Calculator() {
        def summary(implicit previousPeriods:Seq[TaxYearResults], contribution:Contribution): Option[Summary] = None
        def allowance(contribution:Contribution): Long = 0L
      }
    }
  }
}

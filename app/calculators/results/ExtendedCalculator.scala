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
import calculators.internal.ExtendedSummaryCalculator
import scala.util.{Try, Success, Failure}

protected trait ExtendedCalculator extends BasicCalculator {
  protected def getCalculator(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): ExtendedSummaryCalculator

  override def calculate(implicit previousPeriods:Seq[TaxYearResults],
                         contribution: Contribution): Try[(Summary, DetailsResult)] = Try(
      if (!contribution.isEmpty)
        if (isSupported(contribution) && contribution.definedBenefit >= 0) {
          val calculator = getCalculator(previousPeriods, contribution)
          val results = (calculator.summary, calculator.details)
          results match {
            case (None, _) => throw new RuntimeException("Calculation failed")
            case _ => (results._1.get, results._2)
          }
        } else {
          throw new IllegalArgumentException("Not supported")
        }
      else {
        throw new IllegalArgumentException("Contribution is empty")
      }
  )
}

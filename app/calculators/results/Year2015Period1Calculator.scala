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
import calculators.internal._

/**
  Calculator for 2015, period 1 (up to 8th July 2015)
*/
object Year2015Period1Calculator extends ExtendedCalculator {
  protected def getAnnualAllowanceInPounds: Long = 80000L
  protected def getCalculator(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): ExtendedSummaryCalculator = 
    PeriodCalculator(getAnnualAllowanceInPounds)
  def isSupported(contribution:Contribution): Boolean = contribution.isPeriod1() && !contribution.isEmpty
}

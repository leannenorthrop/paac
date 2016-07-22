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
import calculators._
import calculators.internal.Post2015TaperedAllowanceCalculator
import calculators.internal.ExtendedSummaryCalculator
import config.PaacConfiguration

/**
  Calculator for all years from 2008 to 2013.
*/
protected trait Post2015Calculator extends ExtendedCalculator {
  override def allowance(contribution:Contribution): Long = PaacConfiguration.forYear(contribution.taxPeriodStart.taxYear).getOrElse("annual", 0) * 100L

  def isSupported(contribution:Contribution): Boolean = {
    val start = contribution.taxPeriodStart
    val end = contribution.taxPeriodEnd 
    val periodStartAfter = PensionPeriod(2015, 4, 5)
    val periodEndBefore = PensionPeriod(PensionPeriod.LATEST_YEAR_SUPPORTED, 4, 6)
    start > periodStartAfter && start < periodEndBefore && end > periodStartAfter && end < periodEndBefore
  }

  protected def getAnnualAllowanceInPounds: Long = 0L

  protected def getCalculator(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): ExtendedSummaryCalculator = Post2015TaperedAllowanceCalculator()
}

protected object Post2015Calculator extends Post2015Calculator

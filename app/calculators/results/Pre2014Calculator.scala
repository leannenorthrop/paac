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

/**
  Calculator for all years from 2008 to 2013.
*/
protected trait Pre2014Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long = 50000L

  def isSupported(contribution:Contribution): Boolean = {
    val start = contribution.taxPeriodStart
    val end = contribution.taxPeriodEnd 
    val periodStartAfter = PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5)
    val periodEndBefore = PensionPeriod(2014, 4, 6)
    start > periodStartAfter && start < periodEndBefore && end > periodStartAfter && end < periodEndBefore
  }
}

protected object Pre2014Calculator extends Pre2014Calculator 

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
import models.PensionPeriod._

/**
  Calculator for 2014
*/
protected trait Year2014Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long = 40000L // scalastyle:ignore

  def isSupported(contribution:Contribution): Boolean = {
    val start = contribution.taxPeriodStart
    val end = contribution.taxPeriodEnd
    val periodStartAfter = PensionPeriod(YEAR_2015 - 1, APRIL, TAX_YEAR_START_DAY - 1)
    val periodStartBefore = PensionPeriod(YEAR_2015, APRIL, TAX_YEAR_START_DAY)
    start > periodStartAfter && start < periodStartBefore && end > periodStartAfter && end < periodStartBefore
  }
}

protected object Year2014Calculator extends Year2014Calculator
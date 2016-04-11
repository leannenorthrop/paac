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

import config.PaacConfiguration
import models._
import java.util._

object Year2015Period2Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long = PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period2Calculator")).getOrElse(40000L)
  protected val PERIOD_START_AFTER = new GregorianCalendar(2015, 6, 8)
  protected val PERIOD_END_BEFORE = new GregorianCalendar(2016, 3, 6)

  def isSupported(contribution:Contribution):Boolean = {
    val start = contribution.taxPeriodStart.toCalendar
    val end = contribution.taxPeriodEnd.toCalendar
    start.after(PERIOD_START_AFTER) && start.before(PERIOD_END_BEFORE) && end.after(PERIOD_START_AFTER) && end.before(PERIOD_END_BEFORE)
  }
}

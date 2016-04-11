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

object Year2015Period1Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long = PaacConfiguration.config.flatMap[Long](_.getLong("annualallowances.Year2015Period1Calculator")).getOrElse(80000L)
  def isSupported(contribution:Contribution):Boolean = contribution match {
    case Contribution(TaxPeriod(startYear, startMonth, startDay), TaxPeriod(endYear, endMonth, endDay), _) => {
      val start = new GregorianCalendar(startYear, startMonth, startDay)
      val end = new GregorianCalendar(endYear, endMonth, endDay)
      start.after(new GregorianCalendar(2015, 3, 5)) && 
      start.before(new GregorianCalendar(2015, 6, 9)) && 
      end.after(new GregorianCalendar(2015, 3, 5)) &&
      end.before(new GregorianCalendar(2015, 6, 9))
    }
    case _ => false
  }
}

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

object Pre2014Calculator extends BasicCalculator {
  protected def getAnnualAllowanceInPounds: Long = 50000L
  def isSupported(contribution:Contribution):Boolean = contribution match {
    case Contribution(TaxPeriod(startYear, startMonth, startDay), 
                      TaxPeriod(endYear, endMonth, endDay), _) => {
      val start = new java.util.GregorianCalendar(startYear, startMonth, startDay)
      val end = new java.util.GregorianCalendar(endYear, endMonth, endDay)
      start.after(new java.util.GregorianCalendar(2006, 3, 5)) && 
      start.before(new java.util.GregorianCalendar(2014, 3, 6)) && 
      end.after(new java.util.GregorianCalendar(2006, 3, 5)) &&
      end.before(new java.util.GregorianCalendar(2014, 3, 6))
    }
    case _ => false
  }
}
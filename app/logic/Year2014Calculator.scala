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

object Year2014Calculator extends BasicCalculator {
  protected val annualAllowanceInPounds: Long = 40000
  def isSupported(contribution:Contribution):Boolean = contribution match {
    case Contribution(TaxPeriod(year, _, _ ), _, _) if year == 2014 => true
    case _ => false
  }
}
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

package calculators.internal

import models._
import java.text.NumberFormat

trait DetailsCalculator {
  def details(): DetailsResult = detailsResult

  /**
  When value belongs to calculation then it should end with a semi-colon to support expansion
  */
  protected def detail(key: String, value: String): Unit = detailsResult = detailsResult.copy(fields=detailsResult.fields :+ DetailLine(key, value))
  protected def detail(key: String): String = detailsResult.fields.find(_.name == key).map(_.value).getOrElse("")
  protected def fmt(value: Long): String = if (value == 0) currencyFormatter.format(0) else currencyFormatter.format(value/100)
  protected def currency(value: Long): String = if (value == 0) currencyFormatter.format(0) else currencyFormatter.format(value/100)

  private val currencyFormatter = NumberFormat.getNumberInstance(java.util.Locale.UK)
  private var detailsResult = DetailsResult(Seq[DetailLine]())
}

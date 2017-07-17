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

trait DetailsCalculator {
  /**
   * Returns detailed calculations object for a single pension period.
   */
  def details: DetailsResult =
    detailsResult

  /**
  * Save a description or detail.
  * When value belongs to calculation then it should end with a semi-colon to support expansion.
  */
  def detail(key: String, value: String): Unit = {
    val newFields = Seq(DetailLine(key, value),
                        DetailLine(s"${key}.location", getLocation()))
    this.detailsResult = detailsResult.copy(fields=detailsResult.fields ++ newFields)
  }

  /**
   * Return single detail/description with supplied key name.
   */
  def detail(key: String): String =
    detailsResult.fields.find(_.name == key).map(_.value).getOrElse("")

  protected var detailsResult = DetailsResult(Seq[DetailLine]())

  protected def getLocation(): String = {
    val classIndex = 4
    val stack: Seq[java.lang.StackTraceElement] = new java.lang.Throwable().fillInStackTrace().getStackTrace
    s"""see local file ${stack(classIndex).getFileName()} at ${stack(classIndex).getLineNumber()}
        or https://github.com/hmrc/paac/blob/master/app/calculators/internal/${stack(classIndex).getFileName()}#L${stack(classIndex).getLineNumber()}"""
  }
}

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

package metrics

import java.util.concurrent.TimeUnit

import com.codahale.metrics._

import scala.concurrent.{Await, duration}
import scala.concurrent.duration._
import play.api.Logger
import uk.gov.hmrc.play.graphite.MicroserviceMetrics

trait Metrics {
  def successfulRequest()
  def failedRequest()
  def failedCalculation()
  def processRequest(delta: Long, timeUnit: TimeUnit)
  def calculationTime(delta: Long, timeUnit: TimeUnit)
  def taxCalculated(taxYear: String, taxAmount: Long)
  def unusedAllowanceCalculated(taxYear: String, taxAmount: Long)
}

trait GraphiteMetrics extends Metrics with MicroserviceMetrics {
  Logger.info("[Metrics] Registering metrics...")

  private val SUCCESSFUL_REQUEST = "successful-request"
  private val FAILED_REQUEST = "failed-request"
  private val FAILED_CALCULATION = "failed-calculation"
  private val PROCESS_REQUEST_TIMER = "process-request-timer"
  private val CALCULATION_TIMER = "calculation-timer"
  private val YEAR_TAX_CALCULATED = "tax-calculation-"
  private val YEAR_UNUSED_ALLOWANCE_CALCULATED = "unused-allowance-calculation-"
  private val TAX_CALCULATED = "tax-calculation"
  private val UNUSED_ALLOWANCE_CALCULATED = "unused-allowance-calculation"

  def registry: MetricRegistry = metrics.defaultRegistry

  private val timer = (name: String) => registry.timer(name)
  private val counter = (name: String) => registry.counter(name)
  private val histogram = (name: String) => registry.histogram(name)
  private val increment = (name: String) => { Logger.debug(s"[Metrics][${name}] ${counter(name).getCount()}"); counter(name).inc() }
  private val log = (name: String, delta: Long, timeUnit: TimeUnit) => { Logger.debug(s"[Metrics][${name}] ${delta} ${timeUnit}"); timer(name).update(delta, timeUnit) }
  private val update = (name: String, value: Long) => { Logger.info(f"[Metrics][${name}] ${(value / 100.00)}%2.2f "); histogram(name).update(value) }


  Seq((PROCESS_REQUEST_TIMER, timer),
      (CALCULATION_TIMER, timer),
      (SUCCESSFUL_REQUEST, counter),
      (FAILED_REQUEST, counter),
      (FAILED_CALCULATION, counter),
      (TAX_CALCULATED, histogram),
      (UNUSED_ALLOWANCE_CALCULATED, histogram)
    ) foreach { t => t._2(t._1) }

  override def successfulRequest(): Unit = increment(SUCCESSFUL_REQUEST)

  override def failedRequest(): Unit = increment(FAILED_REQUEST)

  override def failedCalculation(): Unit = increment(FAILED_CALCULATION)

  override def processRequest(delta: Long, timeUnit: TimeUnit): Unit = log(PROCESS_REQUEST_TIMER, delta, timeUnit)

  override def calculationTime(delta: Long, timeUnit: TimeUnit): Unit = log(CALCULATION_TIMER, delta, timeUnit)

  override def taxCalculated(taxYear: String, taxAmount: Long): Unit = {
    update(s"${YEAR_TAX_CALCULATED}${taxYear}", taxAmount)
    update(TAX_CALCULATED, taxAmount)
  }

  override def unusedAllowanceCalculated(taxYear: String, amount: Long): Unit = {
    update(s"${YEAR_UNUSED_ALLOWANCE_CALCULATED}${taxYear}", amount)
    update(UNUSED_ALLOWANCE_CALCULATED, amount)
  }

  Logger.info("[Metrics] Completed metrics registration.")
}
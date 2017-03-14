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

package config

import com.typesafe.config.Config
import play.api.{Application, Configuration, Play}
import uk.gov.hmrc.play.audit.filters.AuditFilter
import uk.gov.hmrc.play.config.{AppName, ControllerConfig, RunMode}
import uk.gov.hmrc.play.http.logging.filters.LoggingFilter
import uk.gov.hmrc.play.microservice.bootstrap.DefaultMicroserviceGlobal
import net.ceedubs.ficus.Ficus._
import uk.gov.hmrc.play.filters.MicroserviceFilterSupport

object ControllerConfiguration extends ControllerConfig {
  lazy val controllerConfigs = Play.current.configuration.underlying.as[Config]("controllers")
}

// scalastyle:off
object MicroserviceAuditFilter extends AuditFilter with AppName with MicroserviceFilterSupport {
  override val auditConnector = MicroserviceAuditConnector
  override def controllerNeedsAuditing(controllerName: String) = ControllerConfiguration.paramsForController(controllerName).needsAuditing
}

object MicroserviceLoggingFilter extends LoggingFilter with MicroserviceFilterSupport {
  override def controllerNeedsLogging(controllerName: String) = ControllerConfiguration.paramsForController(controllerName).needsLogging
}
// scalastyle:on

object MicroserviceGlobal extends DefaultMicroserviceGlobal with RunMode {
  override val auditConnector = MicroserviceAuditConnector

  override def microserviceMetricsConfig(implicit app: Application): Option[Configuration] = app.configuration.getConfig(s"microservice.metrics")

  override val loggingFilter = MicroserviceLoggingFilter

  override val microserviceAuditFilter = MicroserviceAuditFilter

  override val authFilter = None
}

object PaacConfiguration {
  lazy val config:Option[Configuration] = Play.current.configuration.getConfig("microservice.paac")

  def forYear(year: Int): Map[String,Int] = {
    config.map {
      (m) =>
      m.getConfig(s"year_${year}").map {
        (subConfig) =>
        subConfig.entrySet.map {
          (pair) =>
          pair._2.unwrapped match {
            case n: Number => (pair._1, n.intValue())
            case _ => (pair._1, -1)
          }
        }.toMap[String,Int]
      }.getOrElse(Map[String,Int]())
    }.getOrElse(Map[String,Int]())
  }
}

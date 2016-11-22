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

package controllers

import metrics._
import models._
import models.PensionPeriod._
import calculators.PensionAllowanceCalculator
import uk.gov.hmrc.play.microservice.controller.BaseController
import java.util.concurrent.TimeUnit

import play.api.mvc._
import play.api.http.Status
import play.api.data.validation._
import play.api.Logger
import play.api.i18n.{ Messages, MessagesApi }
import play.api.libs.json._
import play.api.mvc.Action
import scala.concurrent.Future
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import scala.util.{Success,Failure}

trait CalculatorController {
  this: Controller with PensionAllowanceCalculator with Metrics =>

  def calculate(): Action[JsValue] = Action.async(parse.json) {
    implicit request =>

    request.body.validate[BackendRequest].fold(
      errors => {
        failedRequest
        Future.successful(BadRequest(Json.obj("status" -> JsNumber(BAD_REQUEST),
                                              "message" -> JsString("Invalid JSON request object."),
                                              "validationErrors" -> JsError.toJson(errors))))
      },
      calculationRequest => {
        val contributions = calculationRequest.contributions
        if (contributions.exists((contribution)=>(contribution.taxPeriodStart.year < EARLIEST_YEAR_SUPPORTED ||
                                                  contribution.taxPeriodEnd.year > LATEST_YEAR_SUPPORTED)) ||
            calculationRequest.startFromYear.getOrElse(EARLIEST_YEAR_SUPPORTED) < EARLIEST_YEAR_SUPPORTED) {
          failedRequest
          Future.successful(BadRequest(Json.obj("status" -> JsNumber(BAD_REQUEST),
                                                "message" -> JsString(s"""Unsupported tax year supplied,
                                                                          |only tax years between ${EARLIEST_YEAR_SUPPORTED} and
                                                                          |${LATEST_YEAR_SUPPORTED} inclusive, are supported.""".stripMargin('|')))))
        } else {
          val startTime = System.currentTimeMillis() // should use cross-cutting concerns and wrap this
          val year = calculationRequest.startFromYear.getOrElse(EARLIEST_YEAR_SUPPORTED)
          val isRegisteredYears = calculationRequest.missingYearsAreRegistered.getOrElse(true)
          val r = calculateAllowances(contributions, true, year, isRegisteredYears)
          calculationTime(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS)

          val jsonBody = Json.toJson(r)
          val f = Future.successful(Ok(Json.obj("status" -> JsNumber(OK),
                                                "message" -> JsString("Valid pension calculation request received."),
                                                "results" -> jsonBody)))
          f.onComplete {
            case Success(v) => {
              successfulRequest
              r.foreach { (result) =>
                val ty = result.input.taxYearLabel.replaceAll(" ", "-") + (if (result.input.isTriggered) "-t" else "-nt")
                if (result.summaryResult.chargableAmount > 0)
                  taxCalculated(ty, result.summaryResult.chargableAmount)
                else
                  unusedAllowanceCalculated(ty, result.summaryResult.unusedAllowance.max(result.summaryResult.unusedAAA))
              }
              processRequest(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS)
            }
            case Failure(e) => {
              failedCalculation
              Logger.warn(s"Failed to handle request, ${e.getMessage}. ${e}")
            }
          }
          f
        }
      }
    )
  }
}

object CalculatorController extends Controller with CalculatorController with PensionAllowanceCalculator with GraphiteMetrics

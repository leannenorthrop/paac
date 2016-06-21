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

import models._
import models.PensionPeriod._
import calculators.PensionAllowanceCalculator
import uk.gov.hmrc.play.microservice.controller.BaseController

import play.api.mvc._
import play.api.http.Status
import play.api.data.validation._
import play.api.Logger
import play.api.i18n.{ Messages, MessagesApi }
import play.api.libs.json._
import play.api.mvc.Action
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits._

trait CalculatorController {
  this: Controller with PensionAllowanceCalculator =>

  def calculate() = Action.async(parse.json) { 
    implicit request =>

    request.body.validate[BackendRequest].fold(
      errors => {
        Future.successful(BadRequest(Json.obj("status" -> JsNumber(BAD_REQUEST),
                                              "message" -> JsString("Invalid JSON request object."),
                                              "validationErrors" -> JsError.toFlatJson(errors))))
      },
      calculationRequest => {
        val contributions = calculationRequest.contributions
        if (contributions.exists((contribution)=>(contribution.taxPeriodStart.year < EARLIEST_YEAR_SUPPORTED || 
                                                  contribution.taxPeriodEnd.year > LATEST_YEAR_SUPPORTED)) ||
            calculationRequest.startFromYear.getOrElse(EARLIEST_YEAR_SUPPORTED) < EARLIEST_YEAR_SUPPORTED)
          Future.successful(BadRequest(Json.obj("status" -> JsNumber(BAD_REQUEST),
                                                "message" -> JsString(s"Unsupported tax year supplied, only tax years between ${EARLIEST_YEAR_SUPPORTED} and ${LATEST_YEAR_SUPPORTED} inclusive, are supported."))))
        else
          Future.successful(Ok(Json.obj("status" -> JsNumber(OK), 
                                        "message" -> JsString("Valid pension calculation request received."),
                                        "results" -> Json.toJson(calculateAllowances(contributions, true, calculationRequest.startFromYear.getOrElse(EARLIEST_YEAR_SUPPORTED), calculationRequest.missingYearsAreRegistered.getOrElse(true))))))
      }
    )
  }
}

object CalculatorController extends Controller with CalculatorController with PensionAllowanceCalculator

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

import models.{PensionInput}
import play.api.mvc._
import uk.gov.hmrc.play.microservice.controller.BaseController

import play.api.Logger
import play.api.i18n.Messages
import play.api.libs.json._
import play.api.mvc.Action
import scala.concurrent.Future

/**
  * Created by peri on 02/03/16.
  */
trait CalculatorController {
  this: Controller =>

  def calculate() = Action.async(parse.json) { 
    implicit request =>

    request.body.validate[PensionInput].fold(
      error => {
        Future.successful(BadRequest(Json.obj("status" -> JsNumber(400),
                                              "message" -> JsString("Invalid JSON request object."))))
      },
      result => {
        Future.successful(Ok(Json.obj("status" -> JsNumber(200), 
                                      "message" -> JsString("Valid pension calculation request received."))))
      }
    )
  }
}

object CalculatorController extends Controller with CalculatorController {
}
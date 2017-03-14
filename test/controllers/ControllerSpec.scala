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

package controllers

import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.mvc.{AnyContentAsJson,Request}
import concurrent._
import play.api.libs.json._
import play.api.mvc.{Result, Results, Controller, Action}

import uk.gov.hmrc.play.test.UnitSpec

import org.scalatest._
import org.scalatest.concurrent._

class ControllerSpec extends UnitSpec {
  def getRequestWithJsonBody(path :String, json :JsValue) : Request[JsValue] = {
    FakeRequest(POST, path).withBody(json)
  }
} 

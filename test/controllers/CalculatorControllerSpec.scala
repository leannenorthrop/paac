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
import play.api.test.Helpers._
import play.api.test._
import concurrent._
import play.api.libs.json._
import play.api.mvc.{Result, Results, Controller, Action}
import play.api.mvc.Results._

import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeRequest, FakeApplication}
import models._
import play.api.Play

import uk.gov.hmrc.play.test.UnitSpec

import org.scalatest._
import org.scalatest.concurrent._

class CalculatorControllerSpec extends ControllerSpec with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop()
  }

  val ENDPOINT_PATH = "/paac/calculate/"
  val VALID_CONTRIBUTION_JSON_BODY : List[Contribution] = List[Contribution](Contribution(taxPeriodStart=PensionPeriod(2009, 1, 1), taxPeriodEnd=PensionPeriod(2009, 4, 31), amounts=Some(InputAmounts(90000L,0L))))
  val INVALID_CONTRIBUTION_JSON_BODY : List[Contribution] = List[Contribution](Contribution(taxPeriodStart=PensionPeriod(2009, 1, 1), taxPeriodEnd=PensionPeriod(2009, 4, 31), amounts=Some(InputAmounts(-2000L,0L))))

  def execute(body : List[Contribution]) : Future[Result] = controllers.CalculatorController.calculate()(getRequestWithJsonBody(ENDPOINT_PATH, Json.toJson(body)))

  "Calculator API" should {
    "with valid json request body" must {
      "return 200 OK" in {
          // setup
          val requestBody = VALID_CONTRIBUTION_JSON_BODY

          // do it
          val result = execute(requestBody)

          // check
          status(result) shouldBe OK
      }

      "return appropriate JSON message" in {
          // setup
          val requestBody = VALID_CONTRIBUTION_JSON_BODY

          // do it
          val result = execute(requestBody)

          // check
          (contentAsJson(result) \ "status") shouldBe JsNumber(200)
          (contentAsJson(result) \ "message") shouldBe JsString("Valid pension calculation request received.")
      }
    }

    "with invalid json request body" must {
      "return BadRequest" in {
          // setup
          val requestBody = INVALID_CONTRIBUTION_JSON_BODY

          // do it
          val result = execute(requestBody)

          // check
          status(result) shouldBe status(BadRequest)
      }

      "return error message" in {
        //setup
        val requestBody = INVALID_CONTRIBUTION_JSON_BODY

        //do it
        val result = execute(requestBody)

        //check
        val obj : JsObject = contentAsJson(result).as[JsObject]
        (obj \ "status") shouldBe JsNumber(400)
        (obj \ "message") shouldBe JsString("Invalid JSON request object.")
      }
    }

    "with tax year greater than latest supported tax year" in {
      val contribution0 = Contribution(4400, 5000)
      val contributions = List(contribution0)

      val result = execute(contributions)

      status(result) shouldBe 400
      val results = contentAsJson(result)
      (results \ "status") shouldBe JsNumber(400)
      (results \ "message") shouldBe JsString("Unsupported tax year supplied, only tax years between 2008 and 2016 inclusive, are supported.")
    }

  }
}

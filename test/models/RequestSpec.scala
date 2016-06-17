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

package models

import play.api.test.Helpers._
import play.api.test._
import play.api.libs.json._
import play.api.data.validation._

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class RequestSpec extends ModelSpec {
  "CalculationRequest" should {
    "have a list of contributions" in {
      // set up
      val contributions = List(Contribution(2010,123), Contribution(2011,456))

      // check
      CalculationRequest(contributions, None, None).contributions shouldBe (contributions)
    }    

    "have a startFromYear" in {
      // set up
      val contributions = List()

      // check
      CalculationRequest(contributions, Some(2015), None).startFromYear shouldBe (Some(2015))
    }

    "have a missingYearsAreRegistered" in {
      // set up
      val contributions = List()

      // check
      CalculationRequest(contributions, None, Some(false)).missingYearsAreRegistered shouldBe (Some(false))
    }

    "marshal to Json" in {
      // set up
      val contributions = List(Contribution(2010,123), Contribution(2011,456))
      val r = CalculationRequest(contributions, Some(2012), Some(false))

      // test
      val json = Json.toJson(r)

      // check
      val jsonContributions = json \ "contributions"
      jsonContributions.as[Seq[Contribution]] shouldBe contributions
      val jsonStartFromYear = json \ "startFromYear"
      jsonStartFromYear.as[Int] shouldBe 2012
      val jsonMissingYearsAreRegistered = json \ "missingYearsAreRegistered"
      jsonMissingYearsAreRegistered.as[Boolean] shouldBe false
    }

    "unmarshall from Json" in {
      // set up
      val json = Json.parse("""{"contributions": [], "startFromYear": 2012, "missingYearsAreRegistered" : false}""")

      // test
      val maybeRequest : Option[BackendRequest] = json.validate[BackendRequest].fold(invalid = { e => { println(e);None} }, valid = { r => Some(r) })

      // check
      maybeRequest shouldBe Some(CalculationRequest(List(), Some(2012), Some(false)))
    }

    "toTuple" in {
      // set up
      val contributions = List(Contribution(2010,123), Contribution(2011,456))
      val r = CalculationRequest(contributions, Some(2012), Some(false))

      // test
      val result = BackendRequest.toTuple(r)

      // check
      result._1 shouldBe contributions
      result._2 shouldBe Some(2012)
      result._3 shouldBe Some(false)
    }

    "toRequest" in {
      // set up
      val contributions = List(Contribution(2010,123), Contribution(2011,456))
      val r = CalculationRequest(contributions, Some(2012), Some(false))

      // test
      val result = BackendRequest.toRequest(contributions, Some(2012), Some(false))

      // check
      result shouldBe r
    }
  }
}

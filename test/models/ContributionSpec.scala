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

import org.scalatest._
import org.scalatest.Matchers._

class ContributionSpec extends ModelSpec {
  "A Contribution" can {
    "have a tax year" in {
      // setup
      val taxYear:Short = 2013

      // do it
      val contribution = Contribution(taxYear, 201000)

      // check
      contribution.taxYear shouldBe taxYear
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val contribution = Contribution(taxYear, 201000)

      // do it
      val json = Json.toJson(contribution)

      // check
      val jsonTaxYear = json \ "taxYear"
      jsonTaxYear.as[Short] shouldBe taxYear
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"taxYear": 2012, "amounts": 201003}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(2012, 201003))
    }
  }
}

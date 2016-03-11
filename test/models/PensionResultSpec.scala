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

class PensionResultSpec extends ModelSpec {
  "SummaryResult" can {
    "have default value of 0 for all fields" in {
      // do it
      val summaryResult = SummaryResult()

      // check
      summaryResult.chargableAmount shouldBe 0
      summaryResult.exceedingAAAmount shouldBe 0
    }

    "have chargable amount in pounds" in {
      // setup
      val chargableAmount = 13492

      // do it
      val summaryResult = SummaryResult(chargableAmount=chargableAmount)

      // check
      summaryResult.chargableAmount shouldBe chargableAmount
    }

    "have exceeding Annual Allowance Amount" in {
      // setup
      val exceedingAAAmount = 13492

      // do it
      val summaryResult = SummaryResult(exceedingAAAmount=exceedingAAAmount)

      // check
      summaryResult.exceedingAAAmount shouldBe exceedingAAAmount 
    }

    "marshall to JSON" in {
      // setup
      val chargableAmount : Long = 2468
      val exceedingAAAmount : Long = 13579
      val summaryResult = SummaryResult(chargableAmount, exceedingAAAmount)

      // do it
      val json = Json.toJson(summaryResult)

      // check
      val jsonChargableAmount = json \ "chargableAmount"
      jsonChargableAmount.as[Long] shouldBe chargableAmount
      val jsonExceedingAAAmount = json \ "exceedingAAAmount"
      jsonExceedingAAAmount.as[Long] shouldBe exceedingAAAmount
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"chargableAmount": 12345, "exceedingAAAmount": 67890}""")

      // do it
      val summaryResultOption : Option[SummaryResult] = json.validate[SummaryResult].fold(invalid = { _ => None }, valid = { obj => Some(obj)})

      summaryResultOption shouldBe Some(SummaryResult(12345, 67890))
    }
  }

  "TaxYearResults" can {
    "have tax year and input amounts as a contibution" in {
      // setup
      val contribution = Contribution(2011, InputAmounts(1, 2))

      // do it
      val results = TaxYearResults(contribution, SummaryResult())

      // check
      results.input shouldBe contribution
    }

    "have summary results as a SummaryResult" in {
      // setup
      val summary = SummaryResult(12345, 67890)

      // do it
      val results = TaxYearResults(Contribution(2011, InputAmounts()), summary)

      // check
      results.summaryResult shouldBe summary
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val dbAmountInPounds = 39342
      val mpAmountInPounds = 6789234
      val contribution = Contribution(taxYear, InputAmounts(dbAmountInPounds,mpAmountInPounds))

      val chargableAmount : Long = 2468
      val exceedingAAAmount : Long = 13579
      val summaryResult = SummaryResult(chargableAmount, exceedingAAAmount)

      // do it
      val json = Json.toJson(TaxYearResults(contribution, summaryResult))

      // check
      val jsonTaxYear = json \ "input" \ "taxYear"
      jsonTaxYear.as[Short] shouldBe taxYear
      val jsonDefinedBenfitInPounds = json \ "input" \ "amounts" \ "definedBenefit"
      jsonDefinedBenfitInPounds.as[Long] shouldBe dbAmountInPounds
      val jsonMoneyPurchaseInPounds = json \ "input" \ "amounts" \ "moneyPurchase"
      jsonMoneyPurchaseInPounds.as[Long] shouldBe mpAmountInPounds
      val jsonChargableAmount = json \ "summaryResult" \ "chargableAmount"
      jsonChargableAmount.as[Long] shouldBe chargableAmount
      val jsonExceedingAAAmount = json \ "summaryResult" \ "exceedingAAAmount"
      jsonExceedingAAAmount.as[Long] shouldBe exceedingAAAmount
    }    
  }  
}

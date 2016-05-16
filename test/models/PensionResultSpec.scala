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
      summaryResult.availableAllowance shouldBe 0
      summaryResult.unusedAllowance shouldBe 0
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

    "have available Allowance Amount" in {
      // setup
      val availableAllowanceAmount = 13492

      // do it
      val summaryResult = SummaryResult(availableAllowance=availableAllowanceAmount)

      // check
      summaryResult.availableAllowance shouldBe availableAllowanceAmount 
    }

    "have unused Allowance Amount" in {
      // setup
      val unusedAllowanceAmount = 13492

      // do it
      val summaryResult = SummaryResult(unusedAllowance=unusedAllowanceAmount)

      // check
      summaryResult.unusedAllowance shouldBe unusedAllowanceAmount 
    }

    "have available Allowance with Carry Forward Amount" in {
      // setup
      val availableAllowanceWithCFAmount = 13492

      // do it
      val summaryResult = SummaryResult(availableAAWithCF=availableAllowanceWithCFAmount)

      // check
      summaryResult.availableAAWithCF shouldBe availableAllowanceWithCFAmount 
    }

    "have available Allowance with Cumulative Carry Forward Amount" in {
      // setup
      val availableAllowanceWithCCFAmount = 13492

      // do it
      val summaryResult = SummaryResult(availableAAWithCCF=availableAllowanceWithCCFAmount)

      // check
      summaryResult.availableAAWithCCF shouldBe availableAllowanceWithCCFAmount 
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
      val jsonAvailableAllowance = json \ "availableAllowance"
      jsonAvailableAllowance.as[Long] shouldBe 0
      val jsonUnusedAllowance = json \ "unusedAllowance"
      jsonUnusedAllowance.as[Long] shouldBe 0
      val jsonAvailableAllowanceWithCF = json \ "availableAAWithCF"
      jsonAvailableAllowanceWithCF.as[Long] shouldBe 0
      val jsonAvailableAllowanceWithCCF = json \ "availableAAWithCCF"
      jsonAvailableAllowanceWithCCF.as[Long] shouldBe 0
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"chargableAmount": 12345, "exceedingAAAmount": 67890, "availableAllowance":0, "unusedAllowance": 0, "availableAAWithCF": 0, "availableAAWithCCF":0, "unusedAllowanceCF":0}""")

      // do it
      val summaryResultOption : Option[Summary] = json.validate[Summary].fold(invalid = { _ => None }, valid = { obj => Some(obj)})

      summaryResultOption shouldBe Some(SummaryResult(12345, 67890))
    }
  }

  "TaxYearResults" can {
    "have tax year and input amounts as a contibution" in {
      // setup
      val contribution = Contribution(PensionPeriod(2011, 0, 1), PensionPeriod(2011, 4, 31), Some(InputAmounts(1, 2)))

      // do it
      val results = TaxYearResults(contribution, SummaryResult())

      // check
      results.input shouldBe contribution
    }

    "have summary results as a SummaryResult" in {
      // setup
      val summary = SummaryResult(12345, 67890)

      // do it
      val results = TaxYearResults(Contribution(PensionPeriod(2011, 0, 1), PensionPeriod(2011, 4, 31), Some(InputAmounts())), summary)

      // check
      results.summaryResult shouldBe summary
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val dbAmountInPounds = 39342
      val mpAmountInPounds = 6789234
      val contribution = Contribution(PensionPeriod(taxYear, 0, 1), PensionPeriod(taxYear, 4, 31), Some(InputAmounts(dbAmountInPounds,mpAmountInPounds)))

      val chargableAmount : Long = 2468
      val exceedingAAAmount : Long = 13579
      val summaryResult = SummaryResult(chargableAmount, exceedingAAAmount)

      // do it
      val json = Json.toJson(TaxYearResults(contribution, summaryResult))

      // check
      val jsonTaxYear = json \ "input" \ "taxPeriodStart" \ "year"
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

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"input": {"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, 
                                          "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, 
                                          "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}, 
                                "summaryResult": {"chargableAmount": 12345, "exceedingAAAmount": 67890, "availableAllowance":1, "unusedAllowance": 2, "availableAAWithCF": 3, "availableAAWithCCF":4, "unusedAllowanceCF":5}}""")

      // do it
      val taxYearResultsOption : Option[TaxYearResults] = json.validate[TaxYearResults].fold(invalid = { _ => None }, valid = { obj => Some(obj)})

      taxYearResultsOption.get.input.taxPeriodStart.year shouldBe 2008
      taxYearResultsOption.get.summaryResult.chargableAmount shouldBe 12345
      taxYearResultsOption.get.summaryResult.exceedingAAAmount shouldBe 67890
      taxYearResultsOption.get.summaryResult.availableAllowance shouldBe 1
      taxYearResultsOption.get.summaryResult.unusedAllowance shouldBe 2
      taxYearResultsOption.get.summaryResult.availableAAWithCF shouldBe 3
      taxYearResultsOption.get.summaryResult.availableAAWithCCF shouldBe 4
      taxYearResultsOption.get.summaryResult.unusedAllowanceCF shouldBe 5
    }
  }  
}

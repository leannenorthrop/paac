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
  "InputAmounts" can {
    "have default value of 0 for defined benefit in pence and money purchase in pence" in {
      // do it
      val amounts = InputAmounts()

      // check
      amounts.definedBenefit shouldBe 0
      amounts.moneyPurchase shouldBe 0
    }

    "have a defined benefit amount in pence" in {
      // setup
      val definedBenefitInPence : Long = 246813579

      // do it
      val amounts = InputAmounts(definedBenefitInPence)

      // check
      amounts.definedBenefit shouldBe definedBenefitInPence
    }

    "have a money purchase amount in pence" in {
      // setup
      val moneyPurchaseInPence : Long = 135792468

      // do it
      val amounts = InputAmounts(moneyPurchase=moneyPurchaseInPence)

      // check
      amounts.moneyPurchase shouldBe moneyPurchaseInPence
    }

    "marshall to JSON" in {
      // setup
      val definedBenefitInPence : Long = 2468
      val moneyPurchaseInPence : Long = 13579
      val inputAmounts = InputAmounts(definedBenefitInPence, moneyPurchaseInPence)

      // do it
      val json = Json.toJson(inputAmounts)

      // check
      val jsonDefinedBenefitInPence = json \ "definedBenefit"
      jsonDefinedBenefitInPence.as[Long] shouldBe definedBenefitInPence
      val jsonMoneyPurchaseInPence = json \ "moneyPurchase"
      jsonMoneyPurchaseInPence.as[Long] shouldBe moneyPurchaseInPence
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"definedBenefit": 12345, "moneyPurchase": 67890}""")

      // do it
      val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts)})

      inputAmountsOption shouldBe Some(InputAmounts(12345, 67890))
    }
  }

  "A Contribution" can {
    "have a tax year" in {
      // setup
      val taxYear:Short = 2013

      // do it
      val contribution = Contribution(taxYear, InputAmounts())

      // check
      contribution.taxYear shouldBe taxYear
    }

    "have a defined benefit input amount in pence" in {
      // setup
      val taxYear:Short = 2015
      val dbAmountInPence = 39342
      val amountsInPence:InputAmounts = InputAmounts(dbAmountInPence)

      // do it
      val contribution = Contribution(taxYear, amountsInPence)

      // check
      contribution.amounts.definedBenefit shouldBe dbAmountInPence
    }

    "have a money purchase input amount in pence" in {
      // setup
      val taxYear:Short = 2015
      val mpAmountInPence = 6789234
      val amountsInPence:InputAmounts = InputAmounts(moneyPurchase=mpAmountInPence)

      // do it
      val contribution = Contribution(taxYear, amountsInPence)

      // check
      contribution.amounts.moneyPurchase shouldBe mpAmountInPence
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val dbAmountInPence = 39342
      val mpAmountInPence = 6789234
      val contribution = Contribution(taxYear, InputAmounts(dbAmountInPence,mpAmountInPence))

      // do it
      val json = Json.toJson(contribution)

      // check
      val jsonTaxYear = json \ "taxYear"
      jsonTaxYear.as[Short] shouldBe taxYear
      val jsonDefinedBenfitInPence = json \ "amounts" \ "definedBenefit"
      jsonDefinedBenfitInPence.as[Long] shouldBe dbAmountInPence
      val jsonMoneyPurchaseInPence = json \ "amounts" \ "moneyPurchase"
      jsonMoneyPurchaseInPence.as[Long] shouldBe mpAmountInPence
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"taxYear": 2012, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(2012, InputAmounts(12345, 67890)))
    }
  }
}

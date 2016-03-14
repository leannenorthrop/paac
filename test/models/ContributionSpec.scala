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

class ContributionSpec extends ModelSpec {
  "InputAmounts" can {
    "have default value of 0 for defined benefit and money purchase in pounds" in {
      // do it
      val amounts = InputAmounts()

      // check
      amounts.definedBenefit shouldBe 0
      amounts.moneyPurchase shouldBe 0
    }

    "have a defined benefit amount in pounds" in {
      // setup
      val definedBenefitInPounds : Long = 246813579

      // do it
      val amounts = InputAmounts(definedBenefitInPounds)

      // check
      amounts.definedBenefit shouldBe definedBenefitInPounds
    }

    "have a money purchase amount in pounds" in {
      // setup
      val moneyPurchaseInPounds : Long = 135792468

      // do it
      val amounts = InputAmounts(moneyPurchase=moneyPurchaseInPounds)

      // check
      amounts.moneyPurchase shouldBe moneyPurchaseInPounds
    }

    "marshall to JSON" in {
      // setup
      val definedBenefitInPounds : Long = 2468
      val moneyPurchaseInPounds : Long = 13579
      val inputAmounts = InputAmounts(definedBenefitInPounds, moneyPurchaseInPounds)

      // do it
      val json = Json.toJson(inputAmounts)

      // check
      val jsonDefinedBenefitInPounds = json \ "definedBenefit"
      jsonDefinedBenefitInPounds.as[Long] shouldBe definedBenefitInPounds
      val jsonMoneyPurchaseInPounds = json \ "moneyPurchase"
      jsonMoneyPurchaseInPounds.as[Long] shouldBe moneyPurchaseInPounds
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"definedBenefit": 12345, "moneyPurchase": 67890}""")

      // do it
      val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

      inputAmountsOption shouldBe Some(InputAmounts(12345, 67890))
    }

    "unmarshall from JSON allows 0 amounts" in {
      // setup
      val json = Json.parse("""{"definedBenefit": 0, "moneyPurchase": 0}""")

      // do it
      val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

      inputAmountsOption shouldBe Some(InputAmounts(0, 0))
    }

    "unmashall from JSON ensuring defined benefit is not a negative value" in {
      // setup
      val json = Json.parse("""{"definedBenefit": -3, "moneyPurchase": 67890}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[InputAmounts].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/definedBenefit"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 0
    }

    "unmashall from JSON ensuring money purchase is not a negative value" in {
      // setup
      val json = Json.parse("""{"definedBenefit": 3, "moneyPurchase": -67890}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[InputAmounts].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/moneyPurchase"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 0
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

    "have a defined benefit input amount in pounds" in {
      // setup
      val taxYear:Short = 2015
      val dbAmountInPounds = 39342
      val amountsInPounds:InputAmounts = InputAmounts(dbAmountInPounds)

      // do it
      val contribution = Contribution(taxYear, amountsInPounds)

      // check
      contribution.amounts.definedBenefit shouldBe dbAmountInPounds
    }

    "have a money purchase input amount in pounds" in {
      // setup
      val taxYear:Short = 2015
      val mpAmountInPounds = 6789234
      val amountsInPounds:InputAmounts = InputAmounts(moneyPurchase=mpAmountInPounds)

      // do it
      val contribution = Contribution(taxYear, amountsInPounds)

      // check
      contribution.amounts.moneyPurchase shouldBe mpAmountInPounds
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val dbAmountInPounds = 39342
      val mpAmountInPounds = 6789234
      val contribution = Contribution(taxYear, InputAmounts(dbAmountInPounds,mpAmountInPounds))

      // do it
      val json = Json.toJson(contribution)

      // check
      val jsonTaxYear = json \ "taxYear"
      jsonTaxYear.as[Short] shouldBe taxYear
      val jsonDefinedBenfitInPounds = json \ "amounts" \ "definedBenefit"
      jsonDefinedBenfitInPounds.as[Long] shouldBe dbAmountInPounds
      val jsonMoneyPurchaseInPounds = json \ "amounts" \ "moneyPurchase"
      jsonMoneyPurchaseInPounds.as[Long] shouldBe mpAmountInPounds
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"taxYear": 2012, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(2012, InputAmounts(12345, 67890)))
    }

    "unmarshall from JSON allows tax year of 2008" in {
      // setup
      val json = Json.parse("""{"taxYear": 2008, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(2008, InputAmounts(12345, 67890)))
    }

    "unmashall from JSON ensuring tax year must not be less than 2008" in {
      // setup
      val json = Json.parse("""{"taxYear": 1918, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[Contribution].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/taxYear"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 2008
    }
  }

  "Array of contributions" can {
    "marshall to JSON" in {
      // setup
      val contributions = Seq(Contribution(2011, InputAmounts(1,2)),
                              Contribution(2012, InputAmounts(3,4)),
                              Contribution(2013, InputAmounts(5,6)),
                              Contribution(2014, InputAmounts(7,8)))

      // do it
      val json = Json.toJson(contributions)

      // check
      Json.stringify(json) shouldBe """[{"taxYear":2011,"amounts":{"definedBenefit":1,"moneyPurchase":2}},{"taxYear":2012,"amounts":{"definedBenefit":3,"moneyPurchase":4}},{"taxYear":2013,"amounts":{"definedBenefit":5,"moneyPurchase":6}},{"taxYear":2014,"amounts":{"definedBenefit":7,"moneyPurchase":8}}]"""
    }

    "unmarshall from JSON" in {
      // setup
      val expectedContributions = Seq(Contribution(2011, InputAmounts(1,2)),
                              Contribution(2012, InputAmounts(3,4)),
                              Contribution(2013, InputAmounts(5,6)),
                              Contribution(2014, InputAmounts(7,8)))
      val json = Json.parse("""[{"taxYear":2011,"amounts":{"definedBenefit":1,"moneyPurchase":2}},{"taxYear":2012,"amounts":{"definedBenefit":3,"moneyPurchase":4}},{"taxYear":2013,"amounts":{"definedBenefit":5,"moneyPurchase":6}},{"taxYear":2014,"amounts":{"definedBenefit":7,"moneyPurchase":8}}]""")

      // do it
      val contributionsOption : Option[Contribution] = (json(0)).validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})
      contributionsOption shouldBe Some(expectedContributions(0))
    }
  }
}

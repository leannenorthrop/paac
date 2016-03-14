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
  "TaxPeriod" can {
    "have a full year value as short" in {
      // setup
      val year : Short = 2016

      // do it
      val taxPeriod = TaxPeriod(year, 0, 0)

      // check
      taxPeriod.year shouldBe year
    }
    "have a month value as short" in {
      // setup
      val month : Short = 1

      // do it
      val taxPeriod = TaxPeriod(2000, month, 0)

      // check
      taxPeriod.month shouldBe month
    }
    "have a day value as short" in {
      // setup
      val day : Short = 1

      // do it
      val taxPeriod = TaxPeriod(2000, 0, day)

      // check
      taxPeriod.day shouldBe day
    }
    "marshall to JSON" in {
      // setup
      val year : Short = 2016
      val month : Short = 3
      val day : Short = 15
      val taxPeriod = TaxPeriod(year, month, day)

      // do it
      val json = Json.toJson(taxPeriod)

      // check
      val jsonYear = json \ "year"
      jsonYear.as[Short] shouldBe year
      val jsonMonth = json \ "month"
      jsonMonth.as[Short] shouldBe month
      val jsonDay = json \ "day"
      jsonDay.as[Short] shouldBe day
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"year": 2017, "month": 7, "day" : 19}""")

      // do it
      val inputAmountsOption : Option[TaxPeriod] = json.validate[TaxPeriod].fold(invalid = { _ => None }, valid = { period => Some(period) })

      inputAmountsOption shouldBe Some(TaxPeriod(2017, 7, 19))
    }

    "unmashall from JSON ensuring tax period year is not less than 2000" in {
      // setup
      val json = Json.parse("""{"year": 1999, "month": 7, "day" : 19}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[TaxPeriod].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/year"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 2008
    }
  }

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
      val contribution = Contribution(TaxPeriod(taxYear, 0, 1), TaxPeriod(taxYear, 0, 3), InputAmounts())

      // check
      contribution.taxPeriodStart.year shouldBe taxYear
    }

    "have a defined benefit input amount in pounds" in {
      // setup
      val taxYear:Short = 2015
      val dbAmountInPounds = 39342
      val amountsInPounds:InputAmounts = InputAmounts(dbAmountInPounds)

      // do it
      val contribution = Contribution(TaxPeriod(taxYear, 0, 1), TaxPeriod(taxYear, 0, 3), amountsInPounds)

      // check
      contribution.amounts.definedBenefit shouldBe dbAmountInPounds
    }

    "have a money purchase input amount in pounds" in {
      // setup
      val taxYear:Short = 2015
      val mpAmountInPounds = 6789234
      val amountsInPounds:InputAmounts = InputAmounts(moneyPurchase=mpAmountInPounds)

      // do it
      val contribution = Contribution(TaxPeriod(taxYear, 0, 1), TaxPeriod(taxYear, 0, 3), amountsInPounds)

      // check
      contribution.amounts.moneyPurchase shouldBe mpAmountInPounds
    }

    "marshall to JSON" in {
      // setup
      val taxYear:Short = 2013
      val dbAmountInPounds = 39342
      val mpAmountInPounds = 6789234
      val contribution = Contribution(TaxPeriod(taxYear, 0, 1), TaxPeriod(taxYear, 0, 3), InputAmounts(dbAmountInPounds,mpAmountInPounds))

      // do it
      val json = Json.toJson(contribution)

      // check
      val jsonTaxYear = json \ "taxPeriodStart" \ "year"
      jsonTaxYear.as[Short] shouldBe taxYear
      val jsonDefinedBenfitInPounds = json \ "amounts" \ "definedBenefit"
      jsonDefinedBenfitInPounds.as[Long] shouldBe dbAmountInPounds
      val jsonMoneyPurchaseInPounds = json \ "amounts" \ "moneyPurchase"
      jsonMoneyPurchaseInPounds.as[Long] shouldBe mpAmountInPounds
    }

    "unmarshall from JSON" in {
      // setup
      val json = Json.parse("""{"taxPeriodStart": {"year":2012, "month" : 2, "day" : 12}, "taxPeriodEnd": {"year":2012, "month" : 8, "day" : 11}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(TaxPeriod(2012, 2, 12), TaxPeriod(2012, 8, 11), InputAmounts(12345, 67890)))
    }

    "unmarshall from JSON allows tax year of 2008" in {
      // setup
      val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), InputAmounts(12345, 67890)))
    }

    "unmashall from JSON ensuring tax year must not be less than 2008" in {
      // setup
      val json = Json.parse("""{"taxPeriodStart": {"year":1918, "month" : 2, "day" : 12}, "taxPeriodEnd": {"year":1918, "month" : 8, "day" : 11}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[Contribution].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/taxPeriodEnd/year"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 2008
    }
  }

  "Array of contributions" can {
    "marshall to JSON" in {
      // setup
      val contributions = Seq(Contribution(TaxPeriod(2011, 0, 1), TaxPeriod(2011, 0, 3), InputAmounts(1,2)),
                              Contribution(TaxPeriod(2012, 0, 1), TaxPeriod(2012, 0, 3), InputAmounts(3,4)))

      // do it
      val json = Json.toJson(contributions)

      // check
      Json.stringify(json) shouldBe """[{"taxPeriodStart":{"year":2011,"month":0,"day":1},"taxPeriodEnd":{"year":2011,"month":0,"day":3},"amounts":{"definedBenefit":1,"moneyPurchase":2}},{"taxPeriodStart":{"year":2012,"month":0,"day":1},"taxPeriodEnd":{"year":2012,"month":0,"day":3},"amounts":{"definedBenefit":3,"moneyPurchase":4}}]"""
    }

    "unmarshall from JSON" in {
      // setup
      val expectedContributions = Seq(Contribution(TaxPeriod(2011, 0, 1), TaxPeriod(2011, 0, 3), InputAmounts(1,2)),
                                      Contribution(TaxPeriod(2012, 0, 1), TaxPeriod(2012, 0, 3), InputAmounts(3,4)))
      val json = Json.parse("""[{"taxPeriodStart":{"year":2011,"month":0,"day":1},"taxPeriodEnd":{"year":2011,"month":0,"day":3},"amounts":{"definedBenefit":1,"moneyPurchase":2}},{"taxPeriodStart":{"year":2012,"month":0,"day":1},"taxPeriodEnd":{"year":2012,"month":0,"day":3},"amounts":{"definedBenefit":3,"moneyPurchase":4}}]""")

      // do it
      val contributionsOption : Option[Contribution] = (json(0)).validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})
      contributionsOption shouldBe Some(expectedContributions(0))
    }
  }
}

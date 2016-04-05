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

class ContributionSpec extends ModelSpec {
  trait TaxPeriodFixture {
    val year : Int = 2016
    val month : Int = 7
    val day : Int = 18
    val taxPeriod = TaxPeriod(year, month, day)
  }

  "TaxPeriod" can {
    "have a full year value as int" in new TaxPeriodFixture {
      taxPeriod.year shouldBe year
    }

    "have a month value as int" in new TaxPeriodFixture {
      taxPeriod.month shouldBe month
    }

    "have a day value as int" in new TaxPeriodFixture {
      taxPeriod.day shouldBe day
    }

    "marshall to JSON" in new TaxPeriodFixture {
      // do it
      val json = Json.toJson(taxPeriod)

      // check
      val jsonYear = json \ "year"
      jsonYear.as[Int] shouldBe year
      val jsonMonth = json \ "month"
      jsonMonth.as[Int] shouldBe month
      val jsonDay = json \ "day"
      jsonDay.as[Int] shouldBe day
    }

    "unmarshall from JSON" in new TaxPeriodFixture {
      // setup
      val json = Json.parse("""{"year": 2016, "month": 7, "day" : 18}""")

      // do it
      val inputAmountsOption : Option[TaxPeriod] = json.validate[TaxPeriod].fold(invalid = { _ => None }, valid = { period => Some(period) })

      inputAmountsOption shouldBe Some(taxPeriod)
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
      firstValidationError.args(0) shouldBe 2006
    }
  }

  "InputAmounts" can {
    trait InputAmountsFixture {
      val amounts = InputAmounts()
    }

    "have default value of None for defined benefit and money purchase in pounds" in new InputAmountsFixture {
      // check
      amounts.definedBenefit shouldBe None
      amounts.moneyPurchase shouldBe None
    }

    "have a defined benefit amount in pounds" in new InputAmountsFixture {
      // setup
      val definedBenefitInPounds : Long = 246813579

      // do it
      val a = amounts.copy(definedBenefit=Some(definedBenefitInPounds))

      // check
      a.definedBenefit shouldBe Some(definedBenefitInPounds)
    }

    "have a money purchase amount in pounds" in new InputAmountsFixture {
      // setup
      val moneyPurchaseInPounds : Long = 135792468

      // do it
      val a = amounts.copy(moneyPurchase=Some(moneyPurchaseInPounds))

      // check
      a.moneyPurchase shouldBe Some(moneyPurchaseInPounds)
    }

    "marshall to JSON" in new InputAmountsFixture {
      // setup
      val definedBenefitInPounds : Long = 2468
      val moneyPurchaseInPounds : Long = 13579
      val inputAmounts = amounts.copy(definedBenefit=Some(definedBenefitInPounds), moneyPurchase=Some(moneyPurchaseInPounds))

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

    "unmashalling null" can {
      "definedBenefit results in None" in {
        // setup
        val json = Json.parse("""{"definedBenefit": null, "moneyPurchase": 67890}""")

        // do it
        val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

        inputAmountsOption shouldBe Some(InputAmounts(None, Some(67890L)))
      }
      "moneyPurchase results in None" in {
        // setup
        val json = Json.parse("""{"definedBenefit": 737373, "moneyPurchase": null}""")

        // do it
        val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

        inputAmountsOption shouldBe Some(InputAmounts(Some(737373L), None))
      }
      "definedBenefit empty results in None" in {
        // setup
        val json = Json.parse("""{"moneyPurchase": 67890}""")

        // do it
        val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

        inputAmountsOption shouldBe Some(InputAmounts(None, Some(67890L)))
      }
      "moneyPurchase empty results in None" in {
        // setup
        val json = Json.parse("""{"definedBenefit": 737373}""")

        // do it
        val inputAmountsOption : Option[InputAmounts] = json.validate[InputAmounts].fold(invalid = { _ => None }, valid = { inputAmounts => Some(inputAmounts) })

        inputAmountsOption shouldBe Some(InputAmounts(Some(737373L), None))
      }
    }

    "isEmpty" can {
      "return true if both definedBenefit and money purchase are none" in new InputAmountsFixture {
        amounts.isEmpty shouldBe true
      }

      "return false if either definedBenefit or money purchase are some value" in new InputAmountsFixture {
        InputAmounts(898989).isEmpty shouldBe false
        InputAmounts(898989, 8098080).isEmpty shouldBe false
      }
    }
  }

  trait ContributionFixture {
    val taxYear : Int = 2014
    val taxYearEnd : Int = 2015
    val taxPeriodStart = new TaxPeriod(taxYear, 3, 1) // 1st of April
    val taxPeriodEnd = new TaxPeriod(taxYearEnd, 2, 31) // 31st of March
    val definedBenefit = 2000
    val moneyPurchase = 0
    val contribution = Contribution(taxPeriodStart, taxPeriodEnd, Some(InputAmounts(definedBenefit, moneyPurchase)))
  }

  def getExpectedContributionJson():String = {
    """{"taxPeriodStart":{"year":2014,"month":3,"day":1},"taxPeriodEnd":{"year":2015,"month":2,"day":31},"amounts":{"definedBenefit":2000,"moneyPurchase":0}}"""
  }

  "A Contribution" can {
    "have a tax year" in new ContributionFixture {
      // check
      contribution.taxPeriodStart.year shouldBe taxYear
    }

    "have a tax year label" in {
      // set up
      val c = Contribution(2008, 0)

      // do it 
      val label = c.taxYearLabel

      // check
      label shouldBe "2008/09"
    }

    "have a defined benefit input amount in pounds" in new ContributionFixture {
      // setup
      val dbAmountInPounds = 39342
      val amountsInPounds:InputAmounts = InputAmounts(dbAmountInPounds)

      // do it
      val contrib = contribution.copy(amounts=Some(amountsInPounds))

      // check
      contrib.amounts.get.definedBenefit shouldBe Some(dbAmountInPounds)
    }

    "have a money purchase input amount in pounds" in new ContributionFixture {
      // setup
      val mpAmountInPounds = 6789234
      val amountsInPounds:InputAmounts = InputAmounts(moneyPurchase=Some(mpAmountInPounds))

      // do it
      val contrib = contribution.copy(amounts=Some(amountsInPounds))

      // check
      contrib.amounts.get.moneyPurchase shouldBe Some(mpAmountInPounds)
    }

    "marshall to JSON" in new ContributionFixture {
      // do it
      val json = Json.toJson(contribution)

      // check
      val jsonTaxYear = json \ "taxPeriodStart" \ "year"
      jsonTaxYear.as[Int] shouldBe taxYear
      val jsonDefinedBenfitInPounds = json \ "amounts" \ "definedBenefit"
      jsonDefinedBenfitInPounds.as[Long] shouldBe definedBenefit
      val jsonMoneyPurchaseInPounds = json \ "amounts" \ "moneyPurchase"
      jsonMoneyPurchaseInPounds.as[Long] shouldBe moneyPurchase
    }

    "marshall None amounts to JSON" in {
      // do it
      val json = Json.toJson(Contribution(TaxPeriod(2010,3,5),TaxPeriod(2010,3,6),None))

      // check
      val jsonTaxYear = json \ "taxPeriodStart" \ "year"
      jsonTaxYear.as[Int] shouldBe 2010
      val v = json \ "amounts" 
      v.as[Option[InputAmounts]] shouldBe Some(InputAmounts(None,None))
    }

    "unmarshall from JSON" in new ContributionFixture {
      // setup
      val json = Json.parse(getExpectedContributionJson)

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(contribution)
    }

    "unmarshall from JSON allows tax year of 2008" in {
      // setup
      val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

      contributionOption shouldBe Some(Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(12345, 67890))))
    }

    "unmashall from JSON ensuring tax year must not be less than 2006" in {
      // setup
      val json = Json.parse("""{"taxPeriodStart": {"year":1918, "month" : 2, "day" : 12}, "taxPeriodEnd": {"year":1918, "month" : 8, "day" : 11}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890}}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[Contribution].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/taxPeriodEnd/year"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 2006
    }

    "unmashalling null" can {
      "amounts results in None" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": null}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), None))
      }

      "unmarshall from JSON allows null definedBenefit" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": null, "moneyPurchase": 67890}}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(None, Some(67890L)))))
      }

      "unmarshall from JSON allows null moneyPurchase" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": 9898080}}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(Some(9898080L),None))))
      }
    }    
  }

  "Array of contributions" can {
    "marshall to JSON" in new ContributionFixture {
      // setup
      val contributions = Seq(contribution,
                              contribution)
      val expectedJSON = "[" + getExpectedContributionJson()+","+getExpectedContributionJson() + "]"

      // do it
      val json = Json.toJson(contributions)

      // check
      Json.stringify(json) shouldBe expectedJSON
    }

    "unmarshall from JSON" in new ContributionFixture {
      // setup
      val expectedContributions = Seq(contribution, contribution)
      val json = Json.parse("[" + getExpectedContributionJson()+","+getExpectedContributionJson() + "]")

      // do it
      val contributionsOption : Option[Contribution] = (json(0)).validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})
      contributionsOption shouldBe Some(expectedContributions(0))
    }
  }

  "isEmpty" can {
    "return true if both definedBenefit and money purchase are none or amounts is none" in new ContributionFixture {
      Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), None).isEmpty shouldBe true
      Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(None,None))).isEmpty shouldBe true
    }

    "return false if both definedBenefit and money purchase are none" in new ContributionFixture {
      contribution.isEmpty shouldBe false
    }

    "return false if either definedBenefit or money purchase are some value" in new ContributionFixture {
      Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(8980,897797))).isEmpty shouldBe false
      Contribution(TaxPeriod(2008, 2, 11), TaxPeriod(2008, 8, 12), Some(InputAmounts(8980))).isEmpty shouldBe false
    }
  }
}

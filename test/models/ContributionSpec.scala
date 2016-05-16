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
    val taxPeriod = PensionPeriod(year, month, day)
  }

  "PensionPeriod" can {
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
      val inputAmountsOption : Option[PensionPeriod] = json.validate[PensionPeriod].fold(invalid = { _ => None }, valid = { period => Some(period) })

      inputAmountsOption shouldBe Some(taxPeriod)
    }

    "unmashall from JSON ensuring tax period year is not less than 2000" in {
      // setup
      val json = Json.parse("""{"year": 1999, "month": 7, "day" : 19}""")

      // do it
      val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[PensionPeriod].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
      val firstValidationErrorPath = option.head(0)._1
      val firstValidationError = option.head(0)._2(0)

      // check
      firstValidationErrorPath.toString shouldBe "/year"
      firstValidationError.message shouldBe "error.min"
      firstValidationError.args(0) shouldBe 2006
    }

    "<" should {
      "return true if the given period day is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 1, 2)

        // test
        val result = thisPeriod < thatPeriod

        // check
        result shouldBe true
      }

      "return true if the given period month is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 2, 1)

        // test
        val result = thisPeriod < thatPeriod

        // check
        result shouldBe true
      }

      "return true if the given period year is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2016, 1, 1)

        // test
        val result = thisPeriod < thatPeriod

        // check
        result shouldBe true
      }

      "return true if the given period is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 1, 31)

        // test
        val result = thisPeriod < thatPeriod

        // check
        result shouldBe true
      }

      "return false if the given period year is same as the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 1, 1)

        // test
        val result = thisPeriod < thatPeriod

        // check
        result shouldBe false
      }
    }

    ">" should {
      "return true if the given period day is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 1, 2)

        // test
        val result = thatPeriod > thisPeriod

        // check
        result shouldBe true
      }

      "return true if the given period month is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 2, 1)

        // test
        val result = thatPeriod > thisPeriod

        // check
        result shouldBe true
      }

      "return true if the given period year is after the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2016, 1, 1)

        // test
        val result = thatPeriod > thisPeriod

        // check
        result shouldBe true
      }

      "return false if the given period year is same as the period" in {
        // setup
        val thisPeriod = PensionPeriod(2015, 1, 1)
        val thatPeriod = PensionPeriod(2015, 1, 1)

        // test
        val result = thatPeriod > thisPeriod

        // check
        result shouldBe false
      }
    }
    "PensionPeriod(2015,2,24) >= PensionPeriod(2015,7,9)" should {
      "return false" in {
        // setup
        val thisPeriod = PensionPeriod(2015,2,24)
        val thatPeriod = PensionPeriod(2015,7,9)

        // test
        val result = thisPeriod >= thatPeriod

        // check
        result shouldBe false
      }
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
      "return true if definedBenefit, money purchase and income are none" in new InputAmountsFixture {
        amounts.isEmpty shouldBe true
      }

      "return false if either definedBenefit or money purchase are some value" in new InputAmountsFixture {
        InputAmounts(898989).isEmpty shouldBe false
        InputAmounts(898989, 8098080).isEmpty shouldBe false
        InputAmounts(898989, 8098080, 37372).isEmpty shouldBe false
      }
    }
  }

  trait ContributionFixture {
    val taxYear : Int = 2014
    val taxYearEnd : Int = 2015
    val taxPeriodStart = new PensionPeriod(taxYear, 4, 1) // 1st of April
    val taxPeriodEnd = new PensionPeriod(taxYearEnd, 3, 31) // 31st of March
    val definedBenefit = 2000
    val moneyPurchase = 0
    val income = 123
    val contribution = Contribution(taxPeriodStart, taxPeriodEnd, Some(InputAmounts(definedBenefit, moneyPurchase, income)))
  }

  def getExpectedContributionJson():String = {
    """{"taxPeriodStart":{"year":2014,"month":4,"day":1},"taxPeriodEnd":{"year":2015,"month":3,"day":31},"amounts":{"definedBenefit":2000,"moneyPurchase":0,"income":123,"triggered":null}}"""
  }

  "A Contribution" can {
    "have tax year label which" should {
      "have a simple tax year label" in {
        // set up
        val c = Contribution(2008, 0)

        // do it 
        val label = c.taxYearLabel

        // check
        label shouldBe "2008/09"
      }

      "have a 2015 Period 1 tax year label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0)))

        // do it 
        val label = c.taxYearLabel

        // check
        label shouldBe "2015/16 P1"
      }

      "have a 2015 Period 2 tax year label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0)))

        // do it 
        val label = c.taxYearLabel

        // check
        label shouldBe "2015/16 P2"
      }

      "have a non standard 2015 Period 1 tax year label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod(2015, 4, 12), Some(InputAmounts(0,0)))

        // do it 
        val label = c.taxYearLabel

        // check
        label shouldBe "2015/16 P1"
      }

      "have a non standard 2015 Period 2 tax year label" in {
        // set up
        val c = Contribution(PensionPeriod(2015, 9, 12), PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0)))

        // do it 
        val label = c.taxYearLabel

        // check
        label shouldBe "2015/16 P2"
      }
    }

    "label" can {
      "have a simple label" in {
        // set up
        val c = Contribution(2008, 0)

        // do it 
        val label = c.label

        // check
        label shouldBe "08/09   "
      }

      "have a 2015 Period 1 label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0)))

        // do it 
        val label = c.label

        // check
        label shouldBe "15/16 P1 B"
      }

      "have a 2015 Period 2 label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0)))

        // do it 
        val label = c.label

        // check
        label shouldBe "15/16 P2 B"
      }      

      "have a 2015 Period 1 triggered label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(0), Some(0), None, Some(true))))

        // do it 
        val label = c.label

        // check
        label shouldBe "15/16 P1 A"
      }

      "have a 2015 Period 2 triggered label" in {
        // set up
        val c = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(0), Some(0), None, Some(true))))

        // do it 
        val label = c.label

        // check
        label shouldBe "15/16 P2 A"
      }
    }

    "construction and properties" can {
      "have a tax year" in new ContributionFixture {
        // check
        contribution.taxPeriodStart.year shouldBe taxYear
      }
      "apply creates a proper full tax year period" in {
        // set up
        val c = Contribution(2008, 0)

        // check
        c.taxPeriodStart.year shouldBe 2008
        c.taxPeriodStart.month shouldBe 4
        c.taxPeriodStart.day shouldBe 6
        c.taxPeriodEnd.year shouldBe 2009
        c.taxPeriodEnd.month shouldBe 4
        c.taxPeriodEnd.day shouldBe 5
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

      "have a triggered value" in {
        // set up
        val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(0), Some(0), None, Some(true))))

        // do it
        val isTriggered = contribution.isTriggered

        // check
        isTriggered shouldBe true
      }

      "have a non-triggered value" in {
        // set up
        val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(0), Some(0), None, Some(false))))

        // do it
        val isTriggered = contribution.isTriggered

        // check
        isTriggered shouldBe false
      }

      "have a non-triggered value if set to none" in {
        // set up
        val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(0), Some(0), None, None)))

        // do it
        val isTriggered = contribution.isTriggered

        // check
        isTriggered shouldBe false
      }

      "have a non-triggered value by default" in {
        // set up
        val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0)))

        // do it
        val isTriggered = contribution.isTriggered

        // check
        isTriggered shouldBe false
      }
    }

    "isEmpty" can {
      "return true if both definedBenefit and money purchase are none or amounts is none" in new ContributionFixture {
        Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), None).isEmpty shouldBe true
        Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None,None))).isEmpty shouldBe true
      }

      "return false if both definedBenefit and money purchase are none" in new ContributionFixture {
        contribution.isEmpty shouldBe false
      }

      "return false if either definedBenefit or money purchase are some value" in new ContributionFixture {
        Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(8980,897797))).isEmpty shouldBe false
        Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(8980))).isEmpty shouldBe false
      }
    }

    "isPeriod1" can {
      "return true if is period 1" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(0,0))).isPeriod1 shouldBe true
      }
      "return false if is period 1" in {
        Contribution(2014, 123L).isPeriod1 shouldBe false
      }
    }

    "isPeriod2" can {
      "return true if is period 2" in {
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(0,0))).isPeriod2 shouldBe true
      }
      "return false if is period 2" in {
        Contribution(2014, 123L).isPeriod2 shouldBe false
      }
    }

    "isGroup1" can {
      "return true for group 1 contributions" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(4322L))).isGroup1 shouldBe true
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(4322L))).isGroup1 shouldBe true
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(4322L),None,None,None))).isGroup1 shouldBe true
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(4322L),None,None,Some(false)))).isGroup1 shouldBe true
      }

      "return false for non group 1 contributions" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(None, Some(4322L)))).isGroup1 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(None, Some(4322L)))).isGroup1 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(4322L),None,None,Some(true)))).isGroup1 shouldBe false
        Contribution(2012,243L).isGroup1 shouldBe false
      }
    }

    "isGroup2" can {
      "return true if defined contribution is not None and is period 1 or 2" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(None,Some(474789L),None))).isGroup2 shouldBe true
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(None,Some(474789L),None))).isGroup2 shouldBe true
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(12,474789))).isGroup2 shouldBe true
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(339,474789))).isGroup2 shouldBe true
      }
      "return false if only defined benefit" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(12))).isGroup2 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(339))).isGroup2 shouldBe false
      }
      "return false if not period 1 or 2" in {
        Contribution(2014, 324L).isGroup2 shouldBe false
        Contribution(2016, 429L).isGroup2 shouldBe false
      }
    }

    "isGroup3" can {
      "return false if only defined benefit and not triggered" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(12), None, None, None))).isGroup3 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(12), None, None, None))).isGroup3 shouldBe false
      }
      "return false if only defined contribution/money purchase and not triggered" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(None, Some(12), None, None))).isGroup3 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(None, Some(12), None, None))).isGroup3 shouldBe false
      }
      "return false if not triggered" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(23), Some(12), None, None))).isGroup3 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(23), Some(12), None, None))).isGroup3 shouldBe false
      }
      "return true if triggered and both definedBenefit and moneyPurchase" in {
        Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(23), Some(12), None, Some(true)))).isGroup3 shouldBe false
        Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(23), Some(12), None, Some(true)))).isGroup3 shouldBe true
      }
    }

    "+" should{
      "sum definedBenefit of two contributions regardless of year" in {
        // set up
        val c1 = Contribution(2014, 123L)
        val c2 = Contribution(2014, 456L)

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(579L,0L)))
      }
      "not fail if amounts not defined" in {
        // set up
        val c1 = Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),None)
        val c2 = Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),None)

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe c1
      }
    }

    "JSON" can {
      "marshall to JSON" in new ContributionFixture {
        // do it
        val json = Json.toJson(contribution)

        // check
        val jsonTaxYear = json \ "taxPeriodStart" \ "year"
        jsonTaxYear.as[Int] shouldBe taxYear
        val jsonDefinedBenfit = json \ "amounts" \ "definedBenefit"
        jsonDefinedBenfit.as[Long] shouldBe definedBenefit
        val jsonMoneyPurchase = json \ "amounts" \ "moneyPurchase"
        jsonMoneyPurchase.as[Long] shouldBe moneyPurchase
        val jsonIncome = json \ "amounts" \ "income"
        jsonIncome.as[Long] shouldBe income
      }

      "marshall None amounts to JSON" in {
        // do it
        val json = Json.toJson(Contribution(PensionPeriod(2010,4,5),PensionPeriod(2010,4,6),None))

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

        contributionOption shouldBe Some(Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(12345, 67890))))
      }

      "unmashall from JSON ensuring tax year must not be less than 2006" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":1918, "month" : 2, "day" : 12}, "taxPeriodEnd": {"year":1918, "month" : 8, "day" : 11}, "amounts": {"definedBenefit": 12345, "moneyPurchase": 67890, "income": 372892}}""")

        // do it
        val option : Option[Seq[(play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError])]] = json.validate[Contribution].fold(invalid = { errors => Some(errors) }, valid = { _ => None })
        val firstValidationErrorPath = option.head(0)._1
        val firstValidationError = option.head(0)._2(0)

        // check
        firstValidationErrorPath.toString shouldBe "/taxPeriodEnd/year"
        firstValidationError.message shouldBe "error.min"
        firstValidationError.args(0) shouldBe 2006
      }
    }

    "unmashalling null" can {
      "amounts results in None" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": null}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), None))
      }

      "unmarshall from JSON allows null definedBenefit" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": null, "moneyPurchase": 67890}}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None, Some(67890L)))))
      }

      "unmarshall from JSON allows null moneyPurchase" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": 9898080}}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(9898080L),None))))
      }

      "unmarshall from JSON allows null income" in {
        // setup
        val json = Json.parse("""{"taxPeriodStart": {"year":2008, "month" : 2, "day" : 11}, "taxPeriodEnd": {"year":2008, "month" : 8, "day" : 12}, "amounts": {"definedBenefit": 9898080, "income": null}}""")

        // do it
        val contributionOption : Option[Contribution] = json.validate[Contribution].fold(invalid = { _ => None }, valid = { contribution => Some(contribution)})

        contributionOption shouldBe Some(Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(9898080L),None, None))))
      }

      "be added to another contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None, Some(123L))))
        val c2 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(456L), Some(0L))))

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(456L), Some(123L))))
      }

      "be added to another contribution summing defined benefit" in {
        // set up
        val c1 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(123L), None)))
        val c2 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(Some(456L), None)))

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(579L, 0L)))
      }

      "be added to another contribution summing defined contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None, Some(123L))))
        val c2 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None, Some(456L))))

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(0L, 579L)))
      }

      "is not added to another contribution when amounts is None" in {
        // set up
        val c1 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), None);
        val c2 = Contribution(PensionPeriod(2008, 2, 11), PensionPeriod(2008, 8, 12), Some(InputAmounts(None, Some(456L))))

        // test
        val c3 = c1 + c2

        // check
        c3 shouldBe c1
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
}

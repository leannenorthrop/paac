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

package calculators.periods

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import calculators.results.BasicCalculator

class Group3P2CalculatorSpec extends UnitSpec {
  trait TestFixture {
    val annualAllowance = 50000
    implicit val amountsCalculator = BasicCalculator(annualAllowance)
    def p1Contribution(mp:Long):Contribution = {
      Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(definedBenefit=None,moneyPurchase=Some(mp))))
    }    
    def p2Contribution(mp:Long):Contribution = {
      Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(definedBenefit=None,moneyPurchase=Some(mp))))
    }
  }

  "isMPAAApplicable" should {
    "return true if trigger amount > MPA" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 1L).copy(amounts=Some(InputAmounts(definedBenefit=None,moneyPurchase=Some(5000000L))))

      // test
      val result = Group3P2Calculator().isMPAAApplicable

      // check
      result shouldBe true
    }

    "return true if p1 isMPAAApplicable is true" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(p1Contribution(0L),ExtendedSummaryFields(isMPA=true)))
      implicit val contribution = Contribution(2015, 1L)

      // test
      val result = Group3P2Calculator().isMPAAApplicable

      // check
      result shouldBe true
    }

    "return true if p1 exhausts MPAA" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(p1Contribution(2000000L),ExtendedSummaryFields(isMPA=false)))
      implicit val contribution = Contribution(2015, 1L)

      // test
      val result = Group3P2Calculator().isMPAAApplicable

      // check
      result shouldBe true
    }

    "return false if p2 money purchase is under mpa" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = p2Contribution(10L)

      // test
      val result = Group3P2Calculator().isMPAAApplicable

      // check
      result shouldBe false
    }
  }

  "mpist" should {
    "return 0 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 1L)

      // test
      val result = Group3P2Calculator().mpist

      // check
      result shouldBe 0L
    }
  }

  "defaultChargableAmount" should {
    "return 0 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 9100000L)

      // test
      val result = Group3P2Calculator().defaultChargableAmount

      // check
      result shouldBe 0L
    }
  }

  "exceedingAllowance" should {
    "return 9100000 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 9100000L)

      // test
      val result = Group3P2Calculator().exceedingAllowance

      // check
      result shouldBe 9100000L
    }
  }

  "unusedAllowance" should {
    "return 0 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 9100000L)

      // test
      val result = Group3P2Calculator().unusedAllowance

      // check
      result shouldBe 0L
    }

    "return 0 when triggered and no previous results" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(triggered=Some(true))))

      // test
      val result = Group3P2Calculator().unusedAllowance

      // check
      result shouldBe 0L
    }
  }

  "postFlexiSavings" should {
    "return 0 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 123)

      // test
      val result = Group3P2Calculator().postFlexiSavings

      // check
      result shouldBe 0L
    }
  }

  "preFlexiSavings" should {
    "return sum of dc and db if not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 123)

      // test
      val result = Group3P2Calculator().preFlexiSavings

      // check
      result shouldBe 123L
    }

    "return 0 if triggered and no previous" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(moneyPurchase=Some(123L), triggered=Some(true))))

      // test
      val result = Group3P2Calculator().preFlexiSavings

      // check
      result shouldBe 0L
    }

    "return dc if triggered and no previous" in new TestFixture {
      // set up
      val c = Contribution(2015,0).copy(amounts=Some(InputAmounts(definedBenefit=None,moneyPurchase=Some(123L))))
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(c, SummaryResult()))
      implicit val contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(triggered=Some(true))))

      // test
      val result = Group3P2Calculator().preFlexiSavings

      // check
      result shouldBe 123L
    }
  }

  "unusedAAA" should {
    "return 0 when not triggered" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0)

      // test
      val result = Group3P2Calculator().unusedAAA

      // check
      result shouldBe 0L
    }

    "return period1 unusedAAA - definedBenefit" in new TestFixture {
      // set up
      val period1UnusedAAA = 29098
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, 0, 20001,true)
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(c,ExtendedSummaryFields(unusedAAA=period1UnusedAAA,isMPA=true)))
      implicit val contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(definedBenefit=Some(123L),moneyPurchase=Some(123L),triggered=Some(true))))

      // test
      val result = Group3P2Calculator().unusedAAA

      // check
      result shouldBe (period1UnusedAAA-123L)
    }

    "return 0 when triggered and no allowance" in new TestFixture {
      // set up
      val period1UnusedAAA = 5
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, 0, 20001,true)
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(c,ExtendedSummaryFields(unusedAAA=period1UnusedAAA,isMPA=true)))
      implicit val contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(definedBenefit=Some(123L),moneyPurchase=Some(123L),triggered=Some(true))))

      // test
      val result = Group3P2Calculator().unusedAAA

      // check
      result shouldBe 0L
    }
  }

  "aaCCF" should {
    "return 0 when not triggered and no previous" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 123)

      // test
      val result = Group3P2Calculator().aaCCF

      // check
      result shouldBe 0L
    }
  }

  "aaCF" should {
    "return 0 if no previous periods supplied" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 123)

      // test
      val result = Group3P2Calculator().aaCF

      // check
      result shouldBe 0L
    }
  }
}

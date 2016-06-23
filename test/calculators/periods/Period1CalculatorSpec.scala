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
import org.scalacheck.Gen
import calculators.results.BasicCalculator

class Period1CalculatorSpec extends UnitSpec {

  trait TestFixture {
    val annualAllowance = 80000L
    implicit val amountsCalculator = BasicCalculator(annualAllowance)
  }

  "isBefore2015" should {
    "return true if start period is less than 2015" in new TestFixture {
      implicit var previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2012, 3L)
      val result = TaxYearResults(contribution, SummaryResult())
      Period1Calculator().isBefore2015(result) shouldBe true
    }
    "return true if start period is equal to 2015 but not period 1 or period 2" in new TestFixture {
      implicit var previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(PensionPeriod(2015,1,1), PensionPeriod(2015,2,2), Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      Period1Calculator().isBefore2015(result) shouldBe true
    }
    "return false if start period is equal to period 1" in new TestFixture {
      implicit var previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      Period1Calculator().isBefore2015(result) shouldBe false
    }
    "return false if start period is equal to period 2" in new TestFixture {
      implicit var previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      Period1Calculator().isBefore2015(result) shouldBe false
    }
    "return false if start period is greater to 2015" in new TestFixture {
      implicit var previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2016, 3L)
      val result = TaxYearResults(contribution, SummaryResult())
      Period1Calculator().isBefore2015(result) shouldBe false
    }
  }

  "year2014CCF" should {
    "when no previous results return 0" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 123L)

      // test
      val result = Period1Calculator().year2014CCF

      // check
      result shouldBe 0L
    }

    "when previous results return 2014 ccf" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(Contribution(2014,0), SummaryResult(availableAAWithCCF=123L)))
      implicit val contribution = Contribution(2015, 123L)

      // test
      val result = Period1Calculator().year2014CCF

      // check
      result shouldBe 123L
    }
  }

  "dbist" should {
    "when no previous results and triggered return 0" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults](TaxYearResults(Contribution(2015,0), ExtendedSummaryFields(availableAAWithCCF=123L)))
      implicit val contribution = Contribution(2015, 0L).copy(amounts=Some(InputAmounts(triggered=Some(true))))

      // test
      val result = Period1Calculator().dbist

      // check
      result shouldBe 0L
    }
  }

  "exceedingAllowance" should {
    "when triggered and mpa applicable return 0L" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0L).copy(amounts=Some(InputAmounts(triggered=Some(true),moneyPurchase=Some(2100000))))

      // test
      val result = Period1Calculator().exceedingAllowance

      // check
      result shouldBe 0L
    }
  }

  "preFlexiSavings" should {
    "when triggered and no previous values return 0" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0L)

      // test
      val result = Period1Calculator().preFlexiSavings

      // check
      result shouldBe 0L
    }
  }

  "unusedAAA" should {
    "when triggered and ACA > DCA returns P2 AAA" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0L).copy(amounts=Some(InputAmounts(triggered=Some(true),moneyPurchase=Some(2100000))))

      // test
      val result = Period1Calculator().unusedAAA

      // check
      result shouldBe 3000000L
    }
  }

  "preFlexiSavings" should {
    "when triggered and no previous results returns 0" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0L).copy(amounts=Some(InputAmounts(triggered=Some(true),definedBenefit=Some(1),moneyPurchase=Some(1))))

      // test
      val result = Period1Calculator().preFlexiSavings

      // check
      result shouldBe 0L
    }
  }

  "aaCCF" should {
    "when triggered and mpa applicable with no previous results returns max cf" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2015, 0L).copy(amounts=Some(InputAmounts(triggered=Some(true),moneyPurchase=Some(2100000))))

      // test
      val result = Period1Calculator().aaCCF

      // check
      result shouldBe 3000000L
    }
  }
}

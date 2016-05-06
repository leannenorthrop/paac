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

import play.api.Play
import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.results.BasicCalculator

class Group2P2CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks with BeforeAndAfterAll {

  trait TestFixture {
    val annualAllowance = 50000
    val basicCalculator = BasicCalculator(annualAllowance)
    val calculator = Group2P2Calculator(basicCalculator)
    val period2Contribution = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, None)
  }

  "isMPAAApplicable" should {
    "return true if above MPA" in new TestFixture {
      // set up
      val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

      // test
      val result = calculator.isMPAAApplicable(contribution)

      // check
      result shouldBe true
    }

    "return false if below MPA" in new TestFixture {
      // set up
      val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L))))

      // test
      val result = calculator.isMPAAApplicable(contribution)

      // check
      result shouldBe false
    }

    "defined benefit is 0" in new TestFixture {
      // test
      val result = calculator.definedBenefit

      // check
      result shouldBe 0L
    }

    "dbist is 0" in new TestFixture {
      // test
      val result = calculator.dbist

      // check
      result shouldBe 0L
    }

    "mpist is equal to defined contribution" in new TestFixture {
      // set up
      val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L))))

      // test
      val result = calculator.mpist(contribution)

      // check
      result shouldBe 1200L
    }

    "moneyPurchaseAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = calculator.moneyPurchaseAA(Seq[TaxYearResults]())

        // check
        result shouldBe 0L
      }
    }

    "alternativeAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = calculator.alternativeAA(Seq[TaxYearResults]())

        // check
        result shouldBe 0L
      }
    }

    "alternativeChargableAmount" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = calculator.alternativeChargableAmount(Seq[TaxYearResults](), contribution)

        // check
        result shouldBe 1200000L
      }
    }

    "defaultChargableAmount" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = calculator.defaultChargableAmount(Seq[TaxYearResults](), contribution)

        // check
        result shouldBe 1200000L
      }

      "return 0 if period 1 unused AAA > defined contribution" in new TestFixture {
        // set up
        val period1 = Group2Fields(unusedAAA=5000000L)
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))

        // test
        val result = calculator.defaultChargableAmount(Seq[TaxYearResults](TaxYearResults(contribution, period1)), contribution)

        // check
        result shouldBe 0L
      }
    }

    "exceedingAllowance should be 0" in new TestFixture {
      calculator.exceedingAllowance shouldBe 0L
    } 

    "annualAllowance" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = calculator.annualAllowance(Seq[TaxYearResults]())

        // check
        result shouldBe 0L
      }
    }

    "unusedAllowance" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))

        // test
        val result = calculator.unusedAllowance(Seq[TaxYearResults](), contribution)

        // check
        result shouldBe 0L
      }
    }

    "aaCF" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = calculator.aaCF(Seq[TaxYearResults]())

        // check
        result shouldBe 0L
      }
    }

    "cumulativeMP" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = calculator.cumulativeMP(Seq[TaxYearResults](), contribution)

        // check
        result shouldBe 1200000L
      }
    }

    "cumulativeDB" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        val contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = calculator.cumulativeDB(Seq[TaxYearResults](), contribution)

        // check
        result shouldBe 0L
      }
    }

    "unusedAAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = calculator.unusedAAA(Seq[TaxYearResults]())

        // check
        result shouldBe 0L
      }
    }

    "unusedMPAA" should {
      "return 0" in new TestFixture {
        // test
        val result = calculator.unusedMPAA

        // check
        result shouldBe 0L
      }
    }
  }
}

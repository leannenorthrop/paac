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
    implicit val amountsCalculator = BasicCalculator(annualAllowance)
    implicit var previousPeriods = List[TaxYearResults]()
    implicit var contribution = Contribution(2015, 0)
    val period2Contribution = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, None)
  }

  "isMPAAApplicable" should {
    "return true if above MPA" in new TestFixture {
      // set up
      contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

      // test
      val result = Group2P2Calculator().isMPAAApplicable

      // check
      result shouldBe true
    }

    "return false if below MPA" in new TestFixture {
      // set up
      contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L))))

      // test
      val result = Group2P2Calculator().isMPAAApplicable

      // check
      result shouldBe false
    }

    "defined benefit is 0" in new TestFixture {
      // test
      val result = Group2P2Calculator().definedBenefit

      // check
      result shouldBe 0L
    }

    "dbist is 0" in new TestFixture {
      // test
      val result = Group2P2Calculator().dbist

      // check
      result shouldBe 0L
    }

    "mpist is equal to defined contribution" in new TestFixture {
      // set up
      contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L))))

      // test
      val result = Group2P2Calculator().mpist

      // check
      result shouldBe 1200L
    }

    "moneyPurchaseAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = Group1P1Calculator().moneyPurchaseAA

        // check
        result shouldBe 0L
      }
    }

    "alternativeAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = Group2P2Calculator().alternativeAA

        // check
        result shouldBe 0L
      }
    }

    "alternativeChargableAmount" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = Group2P2Calculator().alternativeChargableAmount

        // check
        result shouldBe 1200000L
      }
    }

    "defaultChargableAmount" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = Group2P2Calculator().defaultChargableAmount

        // check
        result shouldBe 1200000L
      }

      "return 0 if period 1 unused AAA > defined contribution" in new TestFixture {
        // set up
        val period1 = ExtendedSummaryFields(unusedAAA=5000000L)
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))
        previousPeriods = List[TaxYearResults](TaxYearResults(contribution, period1))

        // test
        val result = Group2P2Calculator().defaultChargableAmount

        // check
        result shouldBe 0L
      }
    }

    "exceedingAllowance should be 0" in new TestFixture {
      Group2P2Calculator().exceedingAllowance shouldBe 0L
    } 

    "annualAllowance" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = Group2P2Calculator().annualAllowance

        // check
        result shouldBe 0L
      }
    }

    "unusedAllowance" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))

        // test
        val result = Group2P2Calculator().unusedAllowance

        // check
        result shouldBe 0L
      }
    }

    "aaCF" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = Group2P2Calculator().aaCF

        // check
        result shouldBe 0L
      }
    }

    "cumulativeMP" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = Group2P2Calculator().cumulativeMP

        // check
        result shouldBe 1200000L
      }
    }

    "cumulativeDB" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = Group2P2Calculator().cumulativeDB

        // check
        result shouldBe 0L
      }
    }

    "unusedAAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = Group2P2Calculator().unusedAAA

        // check
        result shouldBe 0L
      }
    }

    "unusedMPAA" should {
      "return 0" in new TestFixture {
        // test
        val result = Group2P2Calculator().unusedMPAA

        // check
        result shouldBe 0L
      }
    }
  }
}

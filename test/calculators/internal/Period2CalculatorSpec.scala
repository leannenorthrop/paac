/*
 * Copyright 2017 HM Revenue & Customs
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

package calculators.internal

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.results._

class Period2CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks with PrivateMethodTester {

  trait TestFixture {
    implicit val annualAllowanceInPounds = 50000L
    implicit var previousPeriods = List[TaxYearResults]()
    implicit var contribution = Contribution(2015, 0)
    val period2Contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, None)
  }

  "period1" should {

    val period1Value = PrivateMethod[ExtendedSummaryFields]('period1)

    "return empty results if no previous periods" in new TestFixture {
      val result = new Period2Calculator() invokePrivate period1Value()
      result shouldBe ExtendedSummaryFields()
    }

    "return empty results if no previous period 1 periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2).map(TaxYearResults(_,SummaryResult()))
      val result = new Period2Calculator() invokePrivate period1Value()
      result shouldBe ExtendedSummaryFields()
    }

    "return period1 results if no previous period 1 periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      val result = new Period2Calculator() invokePrivate period1Value()
      result shouldBe ExtendedSummaryFields(1324L)
    }
  }

  "previous" should {
    val previousValue = PrivateMethod[ExtendedSummaryFields]('previous)

    "return empty results if no previous periods" in new TestFixture {
      val result = new Period2Calculator() invokePrivate previousValue()
      result shouldBe ExtendedSummaryFields()
    }

    "return results if previous periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      val result = new Period2Calculator() invokePrivate previousValue()
      result shouldBe ExtendedSummaryFields(2L)
    }
  }

  "isMPAAApplicable" should {
    "return true if above MPA" in new TestFixture {
      // set up
      contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L), None, Some(true))))

      // test
      val result = new Period2Calculator().isMPAAApplicable

      // check
      result shouldBe true
    }

    "return false if below MPA " in new TestFixture {
      // set up
      contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L), None, Some(true))))

      // test
      val result = new Period2Calculator().isMPAAApplicable

      // check
      result shouldBe false
    }
  }

    "defined benefit is 0" in new TestFixture {
      // test
      val result = new Period2Calculator().definedBenefit

      // check
      result shouldBe 0L
    }

    "moneyPurchaseAA" should {
      "return 10000" in new TestFixture {
        // set up
        val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, Some(true))))
        previousPeriods = List[TaxYearResults](TaxYearResults(p1, ExtendedSummaryFields(unusedMPAA=1000000)))

        // test
        val result = new Period2Calculator().moneyPurchaseAA

        // check
        result shouldBe 1000000L
      }
    }

    "alternativeAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = new Period2Calculator().alternativeAA

        // check
        result shouldBe 0L
      }
    }

    "alternativeChargableAmount" should {
      "return defined contribution - period 2 MPA if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L), None, Some(true))))

        // test
        val result = new Period2Calculator().alternativeChargableAmount

        // check
        result shouldBe 1200000L-1000000L
      }
    }

    "dbist is 0" in new TestFixture {
      // test
      val result = new Period2Calculator().dbist

      // check
      result shouldBe 0L
    }

    "defaultChargableAmount" should {
      "return 0 if period 1 unused AAA > defined contribution" in new TestFixture {
        // set up
        val period1 = ExtendedSummaryFields(unusedAAA=5000000L)
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))
        previousPeriods = List[TaxYearResults](TaxYearResults(contribution, period1))

        // test
        val result = new Period2Calculator().defaultChargableAmount

        // check
        result shouldBe 0L
      }
    }

    "exceedingAllowance should be 0" in new TestFixture {
      new Period2Calculator().exceedingAllowance shouldBe 0L
    }

    "unusedAllowance" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(12000L))))

        // test
        val result = new Period2Calculator().unusedAllowance

        // check
        result shouldBe 0L
      }
      "return 0 unused allowance when no previous results and group 3" in {
        // set up
        val calc = new Year2015Period2Calculator() {
          def allowanceInPounds(): Long = 0L
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
          def contribution(): Contribution = Contribution(false, 123L, 456L)
          def test(): Long = _group3Unused
        }

        // test
        val result = calc.test

        // check
        result shouldBe 0L
      }
      "return 0 unused allowance when no previous results" in {
        // set up
        val calc = new Year2015Period2Calculator() {
          def allowanceInPounds(): Long = 0L
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults](TaxYearResults(Contribution(true, 987L, 654L), ExtendedSummaryFields()))
          def contribution(): Contribution = Contribution(false, 123L, 456L)
          override def defaultChargableAmount(): Long = 800000L
          override def alternativeChargableAmount(): Long = 700000L
          def test(): Long = _group3Unused
        }

        // test
        val result = calc.test

        // check
        result shouldBe 0L
      }
    }

    "annualAllowanceCF" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = new Period2Calculator().annualAllowanceCF

        // check
        result shouldBe 0L
      }
    }

    "cumulativeMP" should {
      "return defined contribution if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = new Period2Calculator().cumulativeMP

        // check
        result shouldBe 1200000L
      }
    }

    "cumulativeDB" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200000L))))

        // test
        val result = new Period2Calculator().cumulativeDB

        // check
        result shouldBe 0L
      }
    }

    "unusedAAA" should {
      "return 0 if no previous periods supplied" in new TestFixture {
        // test
        val result = new Period2Calculator().unusedAAA

        // check
        result shouldBe 0L
      }
    }

    "unusedMPAA" should {
      "return 0" in new TestFixture {
        // test
        val result = new Period2Calculator().unusedMPAA

        // check
        result shouldBe 0L
      }
    }

    "mpist" should {
      "returns 0 if group 3 and mpa is not applicable" in {
        // set up
        implicit val annualAllowanceInPounds = 50000L
        implicit val previousPeriods = Seq[TaxYearResults]()
        implicit val contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(100L), Some(0L), None, Some(true))))

        // test
        val result = new Period2Calculator().mpist

        // check
        result shouldBe 0L
      }
      "return defined contribution" in new TestFixture {
        // set up
        contribution = period2Contribution.copy(amounts = Some(InputAmounts(Some(0L), Some(1200L))))

        // test
        val result = new Period2Calculator().mpist

        // check
        result shouldBe 1200L
      }
      "return 0L" in {
        // set up
        val calc = new Year2015Period2Calculator() {
          def allowanceInPounds(): Long = 0L
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
          def contribution(): Contribution = Contribution(false, 123L, 456L)
          override protected lazy val isGroup3: Boolean = true
          override protected lazy val isPeriod1Triggered: Boolean = false
          override def isMPAAApplicable(): Boolean = false
        }

        // test
        val result = calc.mpist()

        // check
        result shouldBe 0L
      }
    }

    "Alternative Chargable Amount" should {
      "return dc - p2mpa when mpa applicable but not applied in period 1" in {
        // set up
        val calc = new Year2015Period2Calculator() {
          def allowanceInPounds(): Long = 0L
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
          def contribution(): Contribution = Contribution(false, 123L, 1200000L)
          override protected lazy val isGroup3: Boolean = false
          override protected lazy val isPeriod1Triggered: Boolean = false
          override def isMPAAApplicable(): Boolean = true
        }

        // test
        val result = calc.alternativeChargableAmount()

        // check
        result shouldBe 200000L
      }
    }

  "isACA" should {
    "return true when ACA is applicable" in {
      // set up
      val calculator = new Year2015Period2Calculator() {
        def allowanceInPounds(): Long = 0
        def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
        def contribution(): Contribution = Contribution(2015, 0).copy(amounts=Some(InputAmounts(triggered=Some(true))))
        override def alternativeChargableAmount(): Long = 50000
        override def defaultChargableAmount(): Long = 0
      }

      // test
      val result = calculator.isACA

      // check
      result shouldBe true
    }

    "return false when ACA is not applicable" in {
      // set up
      val calculator = new Year2015Period2Calculator() {
        def allowanceInPounds(): Long = 0
        def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
        def contribution(): Contribution = Contribution(2015, 0)
        override def alternativeChargableAmount(): Long = 0
        override def defaultChargableAmount(): Long = 50000
      }

      // test
      val result = calculator.isACA

      // check
      result shouldBe false
    }
  }
}

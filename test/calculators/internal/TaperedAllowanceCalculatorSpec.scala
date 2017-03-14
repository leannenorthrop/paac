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
import play.api.Play
import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeApplication}
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

// scalastyle:off magic.number
// scalastyle:off line.size.limit
class TaperedAllowanceCalculatorSpec extends UnitSpec with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop(app)
  }

  "ExtendedSummaryCalculator trait" should {
    "return 0 for all fields" in {
      // set up
      val calc = new ExtendedSummaryCalculator() {
          def allowance(): Long = 0L
          def definedBenefit(): Long  = 0L
          def definedContribution(): Long = 0L
          def annualAllowance(): Long = 0L
          def exceedingAllowance(): Long = 0L
          def unusedAllowance(): Long = 0L
          def annualAllowanceCF(): Long = 0L
          def annualAllowanceCCF(): Long = 0L
          def chargableAmount(): Long = 0L
      }

      // check
      calc.moneyPurchaseAA shouldBe 0L
      calc.alternativeAA shouldBe 0L
      calc.dbist shouldBe 0L
      calc.mpist shouldBe 0L
      calc.alternativeChargableAmount shouldBe 0L
      calc.defaultChargableAmount shouldBe 0L
      calc.cumulativeMP shouldBe 0L
      calc.cumulativeDB shouldBe 0L
      calc.exceedingMPAA shouldBe 0L
      calc.exceedingAAA shouldBe 0L
      calc.unusedAAA shouldBe 0L
      calc.unusedMPAA shouldBe 0L
      calc.preFlexiSavings shouldBe 0L
      calc.postFlexiSavings shouldBe 0L
      calc.isMPAAApplicable shouldBe false
      calc.acaCF shouldBe 0L
      calc.dcaCF shouldBe 0L
      calc.isACA shouldBe false
    }
  }

  "definedBenefit" should {
    "if not triggered return sum of db and dc" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(4500000L),Some(500000L),Some(0L),Some(false))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).definedBenefit

      // check
      results shouldBe 5000000L
    }
  }

  "isTaperingApplicable" should {
    class Test(contribution:Contribution) extends Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution) {
      def test(): Boolean = super.isTaperingApplicable
    }
    class TestWithPrevious(contribution:Contribution, previousPeriods: Seq[TaxYearResults]) extends Post2015TaperedAllowanceCalculator()(previousPeriods, contribution) {
      def test(): Boolean = super.isTaperingApplicable
    }

    "should return true when adjusted income is over 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe true
    }

    "should return false when adjusted income is = 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000000))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe false
    }

    "should return false when adjusted income is < 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(14900000))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe false
    }

    "should be true if triggered in current year and income > 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(triggered=Some(true))))
      val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(15000100L),triggered=Some(false)))), ExtendedSummaryFields())

      // test
      val result = new TestWithPrevious(contribution, Seq(previous)).test

      // check
      result shouldBe true
    }

    "should be true if triggered in previous year and income > 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100L),triggered=Some(true))))
      val previous = TaxYearResults(Contribution(2015, Some(InputAmounts(triggered=Some(true)))), ExtendedSummaryFields())

      // test
      val result = new TestWithPrevious(contribution, Seq(previous)).test

      // check
      result shouldBe true
    }
  }

  "Tapered allowance" should {
    "should be 40K when tapering is not applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(14900000))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be 40K when tapering is applicable and adjusted income is £150,001" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be £39,999 when tapering is applicable and adjusted income is £150,002" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000200))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 3999900L
    }

    "should be £15k when tapering is applicable and adjusted income is £200,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(20000000))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 1500000L
    }

    "should be £10k when tapering is applicable and adjusted income is £250,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(25000000))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 1000000L
    }

    "should be £40k when tapering is applicable and adjusted income is £150,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000000))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "with carry forwards" should {
      "return tapered to £10k when tapering applies and no previous allowances apply" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(income=Some(25000000))))

        // test
        val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowanceCF

        // check
        result shouldBe 1000000L
      }
    }

    "with cumulative carry forwards" should {
      "return 0 when DCA applies" in {
        // set up
        val test = new TaperedAllowanceCalculator() {
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
          def contribution(): Contribution = Contribution(2016, 4500000L)
          override def alternativeChargableAmount(): Long = 0L
          override def defaultChargableAmount(): Long = 5000000L
        }

        // test
        val result = test.annualAllowanceCCF

        // check
        result shouldBe 0L
      }
    }
  }

  "MPA" should {
    "be £10k" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(0))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).moneyPurchaseAA

      // check
      result shouldBe 1000000L
    }
  }

  "AAA" should {
    "if tapering is not applicable, be AA - MPA" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100),triggered=Some(true))))
      val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(0)))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](previous), contribution).alternativeAA

      // check
      result shouldBe 3000000L
    }

    "if tapering is applicable, be tapered AA - MPA" should {
      "should be £29,999 when tapering is applicable and adjusted income is £150,002" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100),triggered=Some(true))))
        val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(15000200)))))

        // test
        val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](previous), contribution).alternativeAA

        // check
        result shouldBe 2999900L
      }

      "should be £5k when tapering is applicable and adjusted income is £200,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100),triggered=Some(true))))
        val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(20000000)))))

        // test
        val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](previous), contribution).alternativeAA

        // check
        result shouldBe 500000L
      }

      "should be £0k when tapering is applicable and adjusted income is £250,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100),income=Some(25000000),triggered=Some(true))))
        val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(25000000)))))

        // test
        val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](previous), contribution).alternativeAA

        // check
        result shouldBe 0L
      }

      "should be £30k when tapering is applicable and adjusted income is £150,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100),triggered=Some(true))))
        val previous = TaxYearResults(Contribution(2016, Some(InputAmounts(income=Some(15000000)))))

        // test
        val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](previous), contribution).alternativeAA

        // check
        result shouldBe 3000000L
      }
    }
  }

  "isMPA" should {
    "return false if money purchase is less than mpa" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe false
    }

    "return false if money purchase is equal to mpa" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000000L))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe false
    }

    "return true if money purchase is more than mpa" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L),triggered=Some(true))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe true
    }
  }

  "unusedMPAA" should {
    "return 0 if not triggered" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 0L
    }

    "return 0 if mpa applicable (and triggered)" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L),triggered=Some(true))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 0L
    }

    "return unused mpa if not mpa applicable (and triggered)" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(true))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 500000L
    }
  }

  "unused AAA" should {
    "if not mpa applicable return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(false))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if not triggered return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1500000L),triggered=Some(false))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if not mpa applicable and triggered return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(true))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if mpa applicable and triggered return unused AAA" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L),triggered=Some(true))))

      // test
      val result = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 3000000L
    }
  }

  "Previous year" should {
    class Test(previous: Seq[TaxYearResults], contribution:Contribution) extends Post2015TaperedAllowanceCalculator()(previous, contribution) {
      def test(): Option[TaxYearResults] = previousYear
    }

    "return results for previous tax year when present" in {
      // set up
      val contribution1 = Contribution(2018, 0)
      val contribution2 = Contribution(2017, 0)
      val tyr = TaxYearResults(contribution2, ExtendedSummaryFields())

      // test
      val result = new Test(Seq(tyr), contribution1).test()

      // check
      result.isDefined shouldBe true
      result.get shouldBe tyr
    }

    "return None when not present" in {
      // set up
      val contribution1 = Contribution(2018, 0)
      val contribution2 = Contribution(2016, 0)
      val tyr = TaxYearResults(contribution2, ExtendedSummaryFields())

      // test
      val result = new Test(Seq(tyr), contribution1).test()

      // check
      result.isDefined shouldBe false
    }
  }

  "Actual Unused" should {
    class Test(previous: Seq[TaxYearResults], contribution:Contribution) extends Post2015TaperedAllowanceCalculator()(previous, contribution) {
      def actualUnusedList() = actualUnused
    }

    "return unused for all years where no trigger has occurred" in {
      // set up
      val c2008 = TaxYearResults(Contribution(2008, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2009 = TaxYearResults(Contribution(2009, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2010 = TaxYearResults(Contribution(2010, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2011 = TaxYearResults(Contribution(2011, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2012 = TaxYearResults(Contribution(2012, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
      val c2015p2 = TaxYearResults(Contribution(false, 0, 800000L), ExtendedSummaryFields(exceedingAAAmount=800000L,unusedAllowance=0L))
      val previousPeriods = Seq(c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      val contribution = Contribution(2016, 3000000L)

      // test
      val results = new Test(previousPeriods, contribution).actualUnusedList

      // check
      results(1) shouldBe ((2015, 0L))
      results(2) shouldBe ((2014, 1000000L))
      results(3) shouldBe ((2013, 700000L))
      results(4) shouldBe ((2012, 0L))
      results(5) shouldBe ((2011, 1000000L))
    }

    "return unused for all years where trigger has occurred in p2 of 2015" in {
      // set up
      val c2008 = TaxYearResults(Contribution(2008, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2009 = TaxYearResults(Contribution(2009, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2010 = TaxYearResults(Contribution(2010, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2011 = TaxYearResults(Contribution(2011, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2012 = TaxYearResults(Contribution(2012, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
      val c2015p2pre = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=0L))
      val c2015p2post = TaxYearResults(Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, 0, 800000L, true), ExtendedSummaryFields(exceedingAAAmount=800000L,unusedAllowance=0L))
      val previousPeriods = Seq(c2015p2post, c2015p2pre, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      val contribution = Contribution(2016, Some(InputAmounts(definedBenefit=Some(3000000L),triggered=Some(true))))

      // test
      val results = new Test(previousPeriods, contribution).actualUnusedList

      // check
      results(1) shouldBe ((2015, 0L))
      results(2) shouldBe ((2014, 1000000L))
      results(3) shouldBe ((2013, 700000L))
      results(4) shouldBe ((2012, 0L))
      results(5) shouldBe ((2011, 1000000L))
    }

    "return unused for all years where trigger has occurred in 2016 contribution is in 2017" in {
      // set up
      val c2008 = TaxYearResults(Contribution(2008, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2009 = TaxYearResults(Contribution(2009, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2010 = TaxYearResults(Contribution(2010, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2011 = TaxYearResults(Contribution(2011, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2012 = TaxYearResults(Contribution(2012, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
      val c2015p2 = TaxYearResults(Contribution(false, 0, 800000L), ExtendedSummaryFields(exceedingAAAmount=800000L,unusedAllowance=0L))
      val c2016preTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(2000000L), triggered=Some(false)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=2000000L))
      val c2016postTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(500000L), triggered=Some(true)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1500000L))
      val previousPeriods = Seq(c2016postTrigger, c2016preTrigger, c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      val contribution = Contribution(2017, 3000000L)

      // test
      val results = new Test(previousPeriods, contribution).actualUnusedList

      // check
      results(1) shouldBe ((2016, 1500000L))
      results(2) shouldBe ((2015, 0L))
      results(3) shouldBe ((2014, 1000000L))
      results(4) shouldBe ((2013, 700000L))
      results(5) shouldBe ((2012, 0L))
      results(6) shouldBe ((2011, 1000000L))
    }

    "return unused for all years where trigger has occurred in 2016" in {
      // set up
      val c2008 = TaxYearResults(Contribution(2008, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2009 = TaxYearResults(Contribution(2009, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2010 = TaxYearResults(Contribution(2010, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2011 = TaxYearResults(Contribution(2011, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2012 = TaxYearResults(Contribution(2012, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
      val c2015p2 = TaxYearResults(Contribution(false, 0, 800000L), ExtendedSummaryFields(exceedingAAAmount=800000L,unusedAllowance=0L))
      val c2016preTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(2000000L), triggered=Some(false)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=2000000L))
      val previousPeriods = Seq(c2016preTrigger, c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      val contribution = Contribution(2016, Some(InputAmounts(definedBenefit=Some(500000L), triggered=Some(true))))

      // test
      val results = new Test(previousPeriods, contribution).actualUnusedList

      // check
      results(0) shouldBe ((2016, 2000000L))
      results(1) shouldBe ((2015, 0L))
      results(2) shouldBe ((2014, 1000000L))
      results(3) shouldBe ((2013, 700000L))
      results(4) shouldBe ((2012, 0L))
      results(5) shouldBe ((2011, 1000000L))
    }

    "return unused where trigger has occurred in 2016" in {
      // set up
      val previousPeriods = Seq[TaxYearResults]()
      val contribution = Contribution(2016, Some(InputAmounts(definedBenefit=Some(500000L), triggered=Some(true))))

      // test
      val results = new Test(previousPeriods, contribution).actualUnusedList

      // check
      results(0) shouldBe ((2016, 4000000L))
    }
  }

  "Previous 3 years unused allowance" should {
    class Test(previous: Seq[TaxYearResults], contribution:Contribution) extends Post2015TaperedAllowanceCalculator()(previous, contribution) {
      def prev3YearsUnusedAllowance() = previous3YearsUnusedAllowance
      def testActualUnused() = actualUnused
    }

    "return correct value when previous year has execeeded allowance" in {
      // set up
      val c2008 = TaxYearResults(Contribution(2008, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2009 = TaxYearResults(Contribution(2009, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2010 = TaxYearResults(Contribution(2010, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2011 = TaxYearResults(Contribution(2011, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2012 = TaxYearResults(Contribution(2012, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
      val c2015p2 = TaxYearResults(Contribution(false, 0, 800000L), ExtendedSummaryFields(exceedingAAAmount=800000L,unusedAllowance=0L))
      val c2016preTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(2000000L), triggered=Some(false)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=2000000L))
      val c2016postTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(500000L), triggered=Some(true)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1500000L))
      val previousPeriods = Seq(c2016postTrigger, c2016preTrigger, c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      val contribution = Contribution(2017, 4000000L)

      // test
      val results = new Test(previousPeriods, contribution).prev3YearsUnusedAllowance

      // check
      results shouldBe 2500000L
    }

    "return correct value when previous and current year has execeeded allowance" in {
      // set up
      val c2012 = TaxYearResults(Contribution(2012, 0L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2013 = TaxYearResults(Contribution(2013, 1000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
      val c2014 = TaxYearResults(Contribution(2014, 15000000L), ExtendedSummaryFields(exceedingAAAmount=2000000L,unusedAllowance=0L))
      val c2015p1 = TaxYearResults(Contribution(true, 0, 1000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=4000000L))
      val c2015p2 = TaxYearResults(Contribution(false, 0, 1000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=3000000L))

      val previousPeriods = Seq(c2015p2, c2015p1, c2015p1, c2014, c2013, c2012)
      val contribution = Contribution(2016, 5000000L)

      // test
      val results = new Test(previousPeriods, contribution).testActualUnused.foldLeft(0L)(_ + _._2)

      // check
      results shouldBe 2000000L
    }
  }

  "postFlexiSavings" should {
    "return 0 when not triggered" in {
      // set up
      val contribution = Contribution(2017, 4000000L)

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).postFlexiSavings

      // check
      results shouldBe 0L
    }
    "return dc + db if triggered" in {
      // set up
      val contribution = Contribution(2017, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).postFlexiSavings

      // check
      results shouldBe 3L
    }
  }

  "isGroupX" should {
    class Test(contribution:Contribution) extends Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution) {
      def isgroup1(): Boolean = isGroup1
      def isgroup2(): Boolean = isGroup2
      def isgroup3(): Boolean = isGroup3
    }

    "group 1" should {
      "return true when contribution is group 1" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(123L))))

        // test
        val results = new Test(contribution).isgroup1

        // check
        results shouldBe true
      }
      "return false when contribution is not group 1" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(123L),triggered=Some(true))))

        // test
        val results = new Test(contribution).isgroup1

        // check
        results shouldBe false
      }
    }

    "group 2" should {
      "return true when contribution is group 2" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(None,Some(345L))))

        // test
        val results = new Test(contribution).isgroup2

        // check
        results shouldBe true
      }
      "return false when contribution is not group 2" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(123L))))

        // test
        val results = new Test(contribution).isgroup2

        // check
        results shouldBe false
      }
    }

    "group 3" should {
      "return true when contribution is group 3" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(123L),Some(345L),triggered=Some(true))))

        // test
        val results = new Test(contribution).isgroup3

        // check
        results shouldBe true
      }
      "return false when contribution is not group 3" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(None,Some(123L))))

        // test
        val results = new Test(contribution).isgroup3

        // check
        results shouldBe false
      }
    }
  }

  "DBIST" should {
    "be £45k when pre-trigger savings are £45k and no taper allowance and no carry forwards" in {
      // set up
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(0L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger), contribution).dbist

      // check
      results shouldBe 4500000L
    }

    "be £4500000 when savings are £45k but not triggered" in {
      // set up
      val preTrigger = Contribution(2016, 4500000L)

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), preTrigger).dbist

      // check
      results shouldBe 4500000L
    }

    "be £0 when pre-trigger savings are £14k and no taper allowance and with carry forwards" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(availableAAWithCCF=6400000L))
      val preTrigger = TaxYearResults(Contribution(2016, 1400000L), ExtendedSummaryFields(unusedAAA=3000000L))
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1800000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).dbist

      // check
      results shouldBe 0L
    }

    "be £15k when pre-trigger savings are £45k and taper allowance present with no carry forwards" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields())
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1500000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).dbist

      // check
      results shouldBe 1500000L
    }

    "be £5k when pre-trigger savings are £45k and taper allowance present with carry forwards" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(availableAAWithCCF=1000000L))
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1500000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).dbist

      // check
      results shouldBe 500000L
    }
  }

  "MPIST" should {
    "return 0 when not triggered" in {
      // set up
      val contribution = Contribution(2016, 0)

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).mpist

      // check
      results shouldBe 0L
    }
    "return 0 when triggered but mpa is not applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L), Some(900000L), None, Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).mpist

      // check
      results shouldBe 0L
    }
    "return £5k when triggered and mpa is applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L), Some(1500000L), None, Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).mpist

      // check
      results shouldBe 500000L
    }
  }

  "ACA (Alternative Chargable Amount)" should {
    "return £20K when taper not applicable" in {
      // set up
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields(unusedAAA=3000000L))
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1500000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger), contribution).alternativeChargableAmount

      // check
      results shouldBe 2000000L
    }
  }

  "DCA (Default Chargable Amount)" should {
    "return 0 if not triggered" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L), Some(1500000L), None, Some(false))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).defaultChargableAmount

      // check
      results shouldBe 0L
    }
    "return £20k when no carry forwards" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(availableAAWithCCF=0L))
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields(unusedAAA=1238200L))
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1500000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).defaultChargableAmount

      // check
      results shouldBe 2000000L
    }
    "return £37,618 when no carry forwards but adjusted income present" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields())
      val preTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(4500000L),income=Some(18523700L)))), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1500000L),triggered=Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).defaultChargableAmount

      // check
      results shouldBe 3761800L
    }
  }

  "Liable to Charge/Chargable Amount" should {
    "if not triggered return the basic chargeable amount" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(4500000L),Some(500000L),Some(0L),Some(false))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).chargableAmount

      // check
      results shouldBe 1000000L
    }

    "if triggered but mpa is not applicable return default chargeable amount" in {
      // set up
      val preTrigger = TaxYearResults(Contribution(2016, 6500000L), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(900000L),None,Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger), contribution).chargableAmount

      // check
      results shouldBe 3400000L
    }

    "if mpa is applicable return the higher of dca and aca (DCA applies)" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(availableAAWithCCF=0L))
      val preTrigger = TaxYearResults(Contribution(2016, 4500000L), ExtendedSummaryFields())
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1500000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).chargableAmount

      // check
      results shouldBe 2000000L
    }

    "if mpa is applicable return the higher of dca and aca (ACA applies)" in {
      // set up
      val period2 = TaxYearResults(Contribution(false, 0, 0), ExtendedSummaryFields(availableAAWithCCF=6400000L))
      val preTrigger = TaxYearResults(Contribution(2016, 1400000L), ExtendedSummaryFields(unusedAAA=3000000L))
      val contribution = Contribution(2016, Some(InputAmounts(Some(0L),Some(1800000L),Some(0L),Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](preTrigger,period2), contribution).chargableAmount

      // check
      results shouldBe 800000L
    }

    "return DCA if MPA is not applicable" in {
      // set up
      val test = new TaperedAllowanceCalculator() {
        def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
        def contribution(): Contribution = Contribution(2016, 4500000L)
        override def isTriggered(): Boolean = true
        override def isMPAAApplicable(): Boolean = false
      }

      // test
      val result = test.chargableAmount

      // check
      result shouldBe 500000L
    }
  }

  "Cumulative MP" should {
    "return previous + contribution when previous supplied" in {
      // set up
      val contribution = Contribution(2017, Some(InputAmounts(123L, 456L)))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](TaxYearResults(Contribution(2016, None), ExtendedSummaryFields(cumulativeMP=888L))), contribution).cumulativeMP

      // check
      results shouldBe 1344L
    }

    "return contribution when no previous supplied" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(123L, 456L)))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).cumulativeMP

      // check
      results shouldBe 456L
    }
  }

  "Cumulative DB" should {
    "return previous + benefit when previous supplied" in {
      // set up
      val contribution = Contribution(2017, Some(InputAmounts(123L, 456L)))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](TaxYearResults(Contribution(2016, None), ExtendedSummaryFields(cumulativeDB=888L))), contribution).cumulativeDB

      // check
      results shouldBe 1467L
    }

    "return benefit when no previous supplied" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(123L, 456L)))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).cumulativeDB

      // check
      results shouldBe 579L
    }
  }

  "Unused annual allowance" should {
    "return 2999877 when mpa is applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(Some(123L), Some(1200000L), None, Some(true))))

      // test
      val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAllowance

      // check
      results shouldBe 2999877L
    }
  }

  " Exceeding Allowances" should {
    "return 0 when DCA applies" in {
      // set up
      val test = new TaperedAllowanceCalculator() {
        def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults]()
        def contribution(): Contribution = Contribution(2016, 4500000L)
        override def alternativeChargableAmount(): Long = 0L
        override def defaultChargableAmount(): Long = 5000000L
      }

      // test
      val result = test.exceedingAllowance

      // check
      result shouldBe 0L
    }

    "of MPA" should {
      "return £2k when dc is above MPA" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(123L), Some(1200000L), None, Some(true))))

        // test
        val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).exceedingMPAA

        // check
        results shouldBe 200000L
      }
    }

    "of AAA" should {
      "return £25k when db is above AAA" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(Some(5500000L), Some(1200000L), None, Some(true))))

        // test
        val results = Post2015TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).exceedingAAA

        // check
        results shouldBe 2500000L
      }
    }
  }
  "isACA" should {
    "return true when ACA is applicable" in {
      // set up
      val calculator = new TaperedAllowanceCalculator() {
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
      val calculator = new TaperedAllowanceCalculator() {
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
// scalastyle:on magic.number
// scalastyle:on line.size.limit

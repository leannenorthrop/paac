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

package calculators.results

import uk.gov.hmrc.play.test.UnitSpec
import play.api.Play
import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeApplication}
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.SummaryResultCalculator

class TaperedAllowanceCalculatorSpec extends UnitSpec with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop()
  }

  "isTaperingApplicable" should {
    class Test(contribution:Contribution) extends TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution) {
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
  }

  "Tapered allowance" should {
    "should be 40K when tapering is not applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(14900000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be 40K when tapering is applicable and adjusted income is £150,001" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be £39,999 when tapering is applicable and adjusted income is £150,002" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000200))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 3999900L
    }

    "should be £15k when tapering is applicable and adjusted income is £200,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(20000000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 1500000L
    }

    "should be £10k when tapering is applicable and adjusted income is £250,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(25000000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 1000000L
    }

    "should be £40k when tapering is applicable and adjusted income is £150,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }
  }

  "MPA" should {
    "be £10k" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(0))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).moneyPurchaseAA

      // check
      result shouldBe 1000000L
    }
  }

  "AAA" should {
    "if tapering is not applicable, be AA - MPA" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(0))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).alternativeAA

      // check
      result shouldBe 3000000L
    }

    "if tapering is applicable, be tapered AA - MPA" should {
      "should be £29,999 when tapering is applicable and adjusted income is £150,002" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000200))))

        // test
        val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).alternativeAA

        // check
        result shouldBe 2999900L
      }

      "should be £5k when tapering is applicable and adjusted income is £200,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(income=Some(20000000))))

        // test
        val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).alternativeAA

        // check
        result shouldBe 500000L
      }

      "should be £0k when tapering is applicable and adjusted income is £250,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(income=Some(25000000))))

        // test
        val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).alternativeAA

        // check
        result shouldBe 0L
      }

      "should be £30k when tapering is applicable and adjusted income is £150,000" in {
        // set up
        val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000000))))

        // test
        val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).alternativeAA

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
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe false
    }

    "return false if money purchase is equal to mpa" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000000L))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe false
    }

    "return true if money purchase is more than mpa" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).isMPAAApplicable

      // check
      result shouldBe true
    }
  }

  "unusedMPAA" should {
    "return 0 if not triggered" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 0L
    }

    "return 0 if mpa applicable (and triggered)" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L),triggered=Some(true))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 0L
    }

    "return unused mpa if not mpa applicable (and triggered)" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(true))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedMPAA

      // check
      result shouldBe 500000L
    }
  }

  "unused AAA" should {
    "if not mpa applicable return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(false))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if not triggered return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1500000L),triggered=Some(false))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if not mpa applicable and triggered return 0" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(500000L),triggered=Some(true))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 0L
    }
    "if mpa applicable and triggered return unused AAA" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(moneyPurchase=Some(1000100L),triggered=Some(true))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).unusedAAA

      // check
      result shouldBe 3000000L
    }
  }

  "Previous year" should {
    class Test(previous: Seq[TaxYearResults], contribution:Contribution) extends TaperedAllowanceCalculator()(previous, contribution) {
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
      implicit val previousPeriods = Seq(c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      implicit val contribution = Contribution(2016, 3000000L)

      // test
      val results = TaperedAllowanceCalculator().actualUnused

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
      implicit val previousPeriods = Seq(c2015p2post, c2015p2pre, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      implicit val contribution = Contribution(2016, Some(InputAmounts(definedBenefit=Some(3000000L),triggered=Some(true))))

      // test
      val results = TaperedAllowanceCalculator().actualUnused

      // check
      results(1) shouldBe ((2015, 0L))
      results(2) shouldBe ((2014, 1000000L))
      results(3) shouldBe ((2013, 700000L))
      results(4) shouldBe ((2012, 0L))
      results(5) shouldBe ((2011, 1000000L))
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
      val c2016postTrigger = TaxYearResults(Contribution(2016, Some(InputAmounts(definedBenefit=Some(500000L), triggered=Some(true)))), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1500000L))
      implicit val previousPeriods = Seq(c2016postTrigger, c2016preTrigger, c2015p2, c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
      implicit val contribution = Contribution(2017, 3000000L)

      // test
      val results = TaperedAllowanceCalculator().actualUnused

      // check
      results(1) shouldBe ((2016, 1500000L))
      results(2) shouldBe ((2015, 0L))
      results(3) shouldBe ((2014, 1000000L))
      results(4) shouldBe ((2013, 700000L))
      results(5) shouldBe ((2012, 0L))
      results(6) shouldBe ((2011, 1000000L))
    }
  }
}

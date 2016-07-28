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

package calculators.internal

import uk.gov.hmrc.play.test.UnitSpec
import models._
import calculators.internal.utilities._
import calculators.results._

// scalastyle:off number.of.methods
// scalastyle:off magic.number
class UtilitiesSpec extends UnitSpec {
  trait TestFixture {
    implicit var contribution = Contribution(PensionPeriod.PERIOD_1_2015_START,
                                            PensionPeriod.PERIOD_1_2015_END,
                                            Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
    implicit var previousPeriods = List[TaxYearResults]()
    implicit val annualAllowanceInPounds = 5000000L
  }

  "package functions" can {
    "isTriggered" should {
      "return true if triggered" in new TestFixture {
        isTriggered shouldBe true
      }

      "return false if not triggered" in new TestFixture {
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START,
                                    PensionPeriod.PERIOD_1_2015_END,
                                    Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        isTriggered shouldBe false
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START,
                                    PensionPeriod.PERIOD_1_2015_END,
                                    Some(InputAmounts(Some(1L), Some(2L), None, None)))
        isTriggered shouldBe false
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START,
                                    PensionPeriod.PERIOD_1_2015_END,
                                    None)
        isTriggered shouldBe false
      }
    }

    "taxResultNotTriggered" should {
      "return true when not triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START,
                             PensionPeriod.PERIOD_1_2015_END,
                             Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        val tr = TaxYearResults(c,SummaryResult())
        isTaxResultNotTriggered(tr) shouldBe true
        val c2 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
        val tr2 = TaxYearResults(c2,SummaryResult())
        isTaxResultNotTriggered(tr2) shouldBe true
      }

      "return false when triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START,
                             PensionPeriod.PERIOD_1_2015_END,
                             Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
        val tr = TaxYearResults(c,SummaryResult())
        isTaxResultNotTriggered(tr) shouldBe false
      }
    }

    "taxResultTriggered" should {
      "return false when not triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START,
                              PensionPeriod.PERIOD_1_2015_END,
                              Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        val tr = TaxYearResults(c,SummaryResult())
        isTaxResultTriggered(tr) shouldBe false
      }

      "return true when triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START,
                             PensionPeriod.PERIOD_1_2015_END,
                             Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
        val tr = TaxYearResults(c,SummaryResult())
        isTaxResultTriggered(tr) shouldBe true
      }
    }

    "maybeExtended" should {
      "return None when results does not contain extended result" in {
        // set up
        val tyr = TaxYearResults(Contribution(2019,0), SummaryResult())

        // test
        val result = maybeExtended(tyr)

        // check
        result shouldBe None
      }
    }

    "Actual Unused" should {
      class Test(previous: Seq[TaxYearResults], contribution:Contribution) extends Period2Calculator()(80000L, previous, contribution) {
        def test() = actualUnusedList(this)(previousPeriods,contribution)
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
        val previousPeriods = Seq(c2015p1, c2015p1, c2014, c2013, c2012, c2011, c2010, c2009, c2008)
        val contribution = Contribution(false, 0, 800000L)

        // test
        val results = new Test(previousPeriods, contribution).test

        // check
        results(0) shouldBe ((2015, 0L))
        results(1) shouldBe ((2014, 1000000L))
        results(2) shouldBe ((2013, 700000L))
        results(3) shouldBe ((2012, 0L))
        results(4) shouldBe ((2011, 1000000L))
      }

      "return unused for all years where no trigger has occurred with only 2 previous years" in {
        // set up
        val c2013 = TaxYearResults(Contribution(2013, 4000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
        val c2014 = TaxYearResults(Contribution(2014, 3000000L), ExtendedSummaryFields(exceedingAAAmount=0L,unusedAllowance=1000000L))
        val c2015p1 = TaxYearResults(Contribution(true, 0, 8500000L), ExtendedSummaryFields(exceedingAAAmount=500000L,unusedAllowance=0L))
        val previousPeriods = Seq(c2015p1, c2015p1, c2014, c2013)
        val contribution = Contribution(false, 0, 0L)

        // test
        val results = new Test(previousPeriods, contribution).test

        // check
        results(0) shouldBe ((2015, 0L))
        results(1) shouldBe ((2014, 1000000L))
        results(2) shouldBe ((2013, 500000L))
      }
    }
  }
}
// scalastyle:on number.of.methods
// scalastyle:on magic.number

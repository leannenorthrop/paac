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

class PeriodCalculatorSpec extends UnitSpec {
  trait TestFixture {
    implicit var contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
    implicit var previousPeriods = List[TaxYearResults]()

    object PeriodCalculator extends PeriodCalculator {
      def definedBenefit(): Long = 0L
      def chargableAmount(): Long = 0L
      def exceedingAllowance(): Long = 0L
      def annualAllowance(): Long = 0L
      def unusedAllowance(): Long = 0L
      def aaCF(): Long = 0L
      def aaCCF(): Long = 0L
    }
  }

  "isTriggered" should {
    "return true if triggered" in new TestFixture {
      PeriodCalculator.isTriggered shouldBe true
    }

    "return false if not triggered" in new TestFixture {
      contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
      PeriodCalculator.isTriggered shouldBe false
      contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      PeriodCalculator.isTriggered shouldBe false
      contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
      PeriodCalculator.isTriggered shouldBe false
    }
  }

  "taxResultNotTriggered" should {
    "return true when not triggered" in new TestFixture {
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
      val tr = TaxYearResults(c,SummaryResult())
      PeriodCalculator.taxResultNotTriggered(tr) shouldBe true
      val c2 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
      val tr2 = TaxYearResults(c2,SummaryResult())
      PeriodCalculator.taxResultNotTriggered(tr2) shouldBe true
    }

    "return false when triggered" in new TestFixture {
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
      val tr = TaxYearResults(c,SummaryResult())
      PeriodCalculator.taxResultNotTriggered(tr) shouldBe false
    }
  }

  "taxResultTriggered" should {
    "return false when not triggered" in new TestFixture {
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
      val tr = TaxYearResults(c,SummaryResult())
      PeriodCalculator.taxResultTriggered(tr) shouldBe false
    }

    "return true when triggered" in new TestFixture {
      val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
      val tr = TaxYearResults(c,SummaryResult())
      PeriodCalculator.taxResultTriggered(tr) shouldBe true
    }
  }

  "isBefore2015" should {
    "return true if start period is less than 2015" in new TestFixture {
      contribution = Contribution(2012, 3L)
      val result = TaxYearResults(contribution, SummaryResult())
      PeriodCalculator.isBefore2015(result) shouldBe true
    }
    "return true if start period is equal to 2015 but not period 1 or period 2" in new TestFixture {
      contribution = Contribution(PensionPeriod(2015,1,1), PensionPeriod(2015,2,2), Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      PeriodCalculator.isBefore2015(result) shouldBe true
    }
    "return false if start period is equal to period 1" in new TestFixture {
      contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      PeriodCalculator.isBefore2015(result) shouldBe false
    }
    "return false if start period is equal to period 2" in new TestFixture {
      contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val result = TaxYearResults(contribution, SummaryResult())
      PeriodCalculator.isBefore2015(result) shouldBe false
    }
    "return false if start period is greater to 2015" in new TestFixture {
      contribution = Contribution(2016, 3L)
      val result = TaxYearResults(contribution, SummaryResult())
      PeriodCalculator.isBefore2015(result) shouldBe false
    }
  }

  "pre2015Results" should {
    "return only results before 2015" in new TestFixture {
      val expected= List(Contribution(2014,1),Contribution(2013,2)).map(TaxYearResults(_,SummaryResult()))
      previousPeriods = List(TaxYearResults(contribution,SummaryResult())) ++ expected
      PeriodCalculator.pre2015Results shouldBe expected
    }
  }

  "period1" should {
    "return empty results if no previous periods" in new TestFixture {
      PeriodCalculator.period1 shouldBe ExtendedSummaryFields()
    }
    "return empty results if no previous period 1 periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2).map(TaxYearResults(_,SummaryResult()))
      PeriodCalculator.period1 shouldBe ExtendedSummaryFields()
    }
    "return period1 results if no previous period 1 periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      PeriodCalculator.period1 shouldBe ExtendedSummaryFields(1324L)
    }
  }

  "previous" should {
    "return empty results if no previous periods" in new TestFixture {
      PeriodCalculator.previous shouldBe ExtendedSummaryFields()
    }
    "return results if previous periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      PeriodCalculator.previous shouldBe ExtendedSummaryFields(2L)
    }
  }

  "previous results" should {
    "return empty results if no previous periods" in new TestFixture {
      PeriodCalculator.previousResults shouldBe None
    }
    "return results if previous periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      PeriodCalculator.previousResults shouldBe Some(TaxYearResults(Contribution(2012,2),ExtendedSummaryFields(2L)))
    }
  }

  "previous amounts" should {
    "return empty results if no previous periods" in new TestFixture {
      PeriodCalculator.previousInputs shouldBe InputAmounts()
    }    
    "return empty results if previous periods with no amounts" in new TestFixture {
      previousPeriods = List(Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, None)).map((c)=>TaxYearResults(c,ExtendedSummaryFields()))
      PeriodCalculator.previousInputs shouldBe InputAmounts()
    }
    "return results if previous periods" in new TestFixture {
      val p2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
      val p1 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1324L), Some(2L), None, None)))
      previousPeriods = List(Contribution(2012,2),Contribution(2016,3),p2,p1).map((c)=>TaxYearResults(c,ExtendedSummaryFields(c.amounts.get.definedBenefit.get)))
      PeriodCalculator.previousInputs shouldBe InputAmounts(2L)
    }
  }
}

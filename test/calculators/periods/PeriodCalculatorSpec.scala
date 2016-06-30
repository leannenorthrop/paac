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
import calculators.periods.Utilities._

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
      def basicCalculator(): calculators.results.BasicCalculator = calculators.results.BasicCalculator(5000000L)
    }
  }

  "package functions" can {
    "isTriggered" should {
      "return true if triggered" in new TestFixture {
        isTriggered shouldBe true
      }

      "return false if not triggered" in new TestFixture {
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        isTriggered shouldBe false
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, None)))
        isTriggered shouldBe false
        contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
        isTriggered shouldBe false
      }
    }

    "taxResultNotTriggered" should {
      "return true when not triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        val tr = TaxYearResults(c,SummaryResult())
        taxResultNotTriggered(tr) shouldBe true
        val c2 = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
        val tr2 = TaxYearResults(c2,SummaryResult())
        taxResultNotTriggered(tr2) shouldBe true
      }

      "return false when triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
        val tr = TaxYearResults(c,SummaryResult())
        taxResultNotTriggered(tr) shouldBe false
      }
    }

    "taxResultTriggered" should {
      "return false when not triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        val tr = TaxYearResults(c,SummaryResult())
        taxResultTriggered(tr) shouldBe false
      }

      "return true when triggered" in new TestFixture {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
        val tr = TaxYearResults(c,SummaryResult())
        taxResultTriggered(tr) shouldBe true
      }
    }
  }
}

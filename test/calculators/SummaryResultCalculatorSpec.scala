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

package calculators

import calculators.results._
import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class SummaryResultCalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks {

  "SummaryResultCalculator" should {
    "return None when no amounts given" in {
      // set up
      val contribution = Contribution(PensionPeriod(2014,4,5), PensionPeriod(2015,4,6), None)
      val calculator = new SummaryResultCalculator(100, Seq[TaxYearResults](), contribution)

      // check
      calculator.summary shouldBe None
    }

    "return 0 for defined benefit if no amounts given" in {
      // set up
      val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
      val calculator = new SummaryResultCalculator(100, Seq[TaxYearResults](), contribution)

      // check
      calculator.definedBenefit shouldBe 0L
    }

    "return 0 for defined contribution if no amounts given" in {
      // set up
      val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
      val calculator = new SummaryResultCalculator(100, Seq[TaxYearResults](), contribution)

      // check
      calculator.definedContribution shouldBe 0L
    }

    "actualUnused" should {
      "return list of tuples containing unused allowance when all defined benefit is less than allowance" in {
        // set up
        val tr1 = TaxYearResults(Contribution(2008, 4000000L), SummaryResult(unusedAllowance=1000000L,availableAllowance=5000000L))
        val tr2 = TaxYearResults(Contribution(2009, 3000000L), SummaryResult(unusedAllowance=2000000L,availableAllowance=5000000L))
        val tr3 = TaxYearResults(Contribution(2010, 2000000L), SummaryResult(unusedAllowance=3000000L,availableAllowance=5000000L))
        val tr4 = TaxYearResults(Contribution(2011, 1000000L), SummaryResult(unusedAllowance=4000000L,availableAllowance=5000000L))
        val previous = Seq(tr4, tr3, tr2, tr1)
        val contribution = Contribution(2012, 5000000L)
        val calculator = new SummaryResultCalculator(50000L, previous, contribution)

        // test
        val results = calculators.Utilities.actualUnusedList(calculator)(previous, contribution)

        // check
        results.length shouldBe 5
        results(0) shouldBe ((2012, 0L))
        results(1) shouldBe ((2011, 4000000L))
        results(2) shouldBe ((2010, 3000000L))
        results(3) shouldBe ((2009, 2000000L))
        results(4) shouldBe ((2008, 1000000L))
      }
    }

    "annualAllowanceCCF" should {
      "return correct 2010 annual allowance cumulative carry forward values when contributions are zero" in {
        // setup
        val tr1 = TaxYearResults(Contribution(2008, 0L), SummaryResult(unusedAllowance=5000000L,availableAllowance=5000000L,exceedingAAAmount=0L))
        val tr2 = TaxYearResults(Contribution(2009, 0L), SummaryResult(unusedAllowance=5000000L,availableAllowance=5000000L,exceedingAAAmount=0L))
        val previous = Seq(tr1, tr2)
        val contribution = Contribution(2010, 0L)
        val calculator = new SummaryResultCalculator(50000L, previous, contribution)

        // test
        val results = calculator.annualAllowanceCCF

        // check
        results shouldBe 15000000L
      }

      "return correct 2011 annual allowance cumulative carry forward values when contributions are zero" in {
        // setup
        val tr1 = TaxYearResults(Contribution(2008, 0L), SummaryResult(unusedAllowance=5000000L,availableAllowance=5000000L,exceedingAAAmount=0L))
        val tr2 = TaxYearResults(Contribution(2009, 0L), SummaryResult(unusedAllowance=5000000L,availableAllowance=5000000L,exceedingAAAmount=0L))
        val tr3 = TaxYearResults(Contribution(2010, 0L), SummaryResult(unusedAllowance=5000000L,availableAllowance=5000000L,exceedingAAAmount=0L))
        val previous = Seq(tr1, tr2)
        val contribution = Contribution(2011, 0L)
        val calculator = new SummaryResultCalculator(50000L, previous, contribution)

        // test
        val results = calculator.annualAllowanceCCF

        // check
        results shouldBe 15000000L
      }
    }

    "allowance" should {
      "return allowance in pounds" in {
        new SummaryResultCalculator(50000L, Seq[TaxYearResults](), Contribution(2000, 0)).allowance shouldBe 50000L
      }
    }
  }
} 
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

package logic

import uk.gov.hmrc.play.test.UnitSpec

import models._

import org.scalatest._

class PensionAllowanceCalculatorSpec extends UnitSpec {
    trait ContributionPre2014Fixture {
      val contribution0 = Contribution(2008, 5000)
      val contribution1 = Contribution(2009, 6000)
      val contribution2 = Contribution(2010, 7000)
      val contribution3 = Contribution(2011, 8000)
      val contribution4 = Contribution(2012, 9000)
      val contribution5 = Contribution(2013, 10000)
    }

  "PensionAllowanceCalculator" should {
    "return 0 for any input" in {
      // set up
      val input = Contribution(TaxPeriod(2016, 3, 1), TaxPeriod(2016, 8, 31), InputAmounts())
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult())
    }

    "return 0 for contributions prior to 2008" in {
      // set up
      val input = Contribution(TaxPeriod(1914, 3, 1), TaxPeriod(1915, 8, 31), InputAmounts())
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult())
    }

    "return correct allowances and carry forward values for contributions prior to 2014" in new ContributionPre2014Fixture {
      // set up
      val inputs = List(contribution0, contribution1, contribution2, contribution3, contribution4, contribution5)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 6
      results(0) shouldBe TaxYearResults(inputs(0), SummaryResult(0,0,50000,45000,50000,45000))
      results(1) shouldBe TaxYearResults(inputs(1), SummaryResult(0,0,50000,44000,95000,89000))
      results(2) shouldBe TaxYearResults(inputs(2), SummaryResult(0,0,50000,43000,139000,132000))
      results(3) shouldBe TaxYearResults(inputs(3), SummaryResult(0,0,50000,42000,182000,129000))
      results(4) shouldBe TaxYearResults(inputs(4), SummaryResult(0,0,50000,41000,179000,126000))
      results(5) shouldBe TaxYearResults(inputs(5), SummaryResult(0,0,50000,40000,176000,123000))
    }
  }

  "Pre2014Calculator" should {
    trait ZeroContributionFixture {
      val contribution = Contribution(TaxPeriod(2009, 3, 1), TaxPeriod(2009, 8, 31), InputAmounts())
    }

    "return none for contributions prior to 2008" in {
      // set up
      val input = Contribution(TaxPeriod(1914, 3, 1), TaxPeriod(1915, 8, 31), InputAmounts())

      // do it
      val results = Pre2014Calculator.summary(Seq[SummaryResult](), input)

      // check it
      results shouldBe None
    }

    "return some for contributions prior to 2014 and after 2007" in new ZeroContributionFixture {
      // do it
      val results = Pre2014Calculator.summary(Seq[SummaryResult](), contribution)

      // check it
      results shouldBe Some(SummaryResult(0,0,50000,50000,50000,50000))
    }

    "return amount exceeding Annual Allowance of 0 for values under 50000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.exceedingAAAmount shouldBe 0
    }

    "return amount chargable amount of 0 for values under 50000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.chargableAmount shouldBe 0
    }

    "return available allowance of 50000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.availableAllowance shouldBe 50000
    }

    "return unused allowance of 44000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.unusedAllowance shouldBe 44000
    }

    "return available allowance with carry forward of 95000" in new ContributionPre2014Fixture {
      // do it
      val starting = SummaryResult(chargableAmount = 0,
                                   exceedingAAAmount = 0,
                                   availableAllowance = 50000,
                                   unusedAllowance = 45000,
                                   availableAAWithCF = 50000,
                                   availableAAWithCCF = 45000)
      val result = Pre2014Calculator.summary(Seq[SummaryResult](starting), contribution1).get

      // check it
      result.chargableAmount shouldBe 0
      result.exceedingAAAmount shouldBe 0
      result.availableAllowance shouldBe 50000
      result.unusedAllowance shouldBe 44000
      result.availableAAWithCF shouldBe 95000
      result.availableAAWithCCF shouldBe 89000
    }
  }
} 
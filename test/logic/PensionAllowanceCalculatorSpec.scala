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
  }

  "Pre2014Calculator" should {
    trait ZeroContributionFixture {
      val contribution = Contribution(TaxPeriod(2013, 3, 1), TaxPeriod(2013, 8, 31), InputAmounts())
    }

    trait ContributionFixture {
      val contribution = Contribution(TaxPeriod(2013, 3, 1), TaxPeriod(2013, 8, 31), InputAmounts(definedBenefit=5000))
    }

    "return none for contributions prior to 2008" in {
      // set up
      val input = Contribution(TaxPeriod(1914, 3, 1), TaxPeriod(1915, 8, 31), InputAmounts())

      // do it
      val results = Pre2014Calculator.summary(input)

      // check it
      results shouldBe None
    }

    "return some for contributions prior to 2014 and after 2007" in new ZeroContributionFixture {
      // do it
      val results = Pre2014Calculator.summary(contribution)

      // check it
      results shouldBe Some(SummaryResult(0,0,50000,50000))
    }

    "return amount exceeding Annual Allowance of 0 for values under 50000" in new ContributionFixture {
      // do it
      val result = Pre2014Calculator.summary(contribution).get

      // check it
      result.exceedingAAAmount shouldBe 0
    }

    "return amount chargable amount of 0 for values under 50000" in new ContributionFixture {
      // do it
      val result = Pre2014Calculator.summary(contribution).get

      // check it
      result.chargableAmount shouldBe 0
    }

    "return available allowance of 50000" in new ContributionFixture {
      // do it
      val result = Pre2014Calculator.summary(contribution).get

      // check it
      result.availableAllowance shouldBe 50000
    }

    "return unused allowance of 45000" in new ContributionFixture {
      // do it
      val result = Pre2014Calculator.summary(contribution).get

      // check it
      result.unusedAllowance shouldBe 45000
    }
  }
} 
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
import org.scalatest.prop._
import org.scalacheck.Gen

class Pre2014CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks {
    trait ContributionPre2014Fixture {
      val contribution0 = Contribution(2008, 500000)
      val contribution1 = Contribution(2009, 600000)
      val contribution2 = Contribution(2010, 700000)
      val contribution3 = Contribution(2011, 800000)
      val contribution4 = Contribution(2012, 900000)
      val contribution5 = Contribution(2013, 1000000)
    }

  "Pre2014Calculator" should {
    trait ZeroContributionFixture {
      val contribution = Contribution(TaxPeriod(2009, 3, 1), TaxPeriod(2009, 8, 31), InputAmounts())
    }

    "not support calculations for tax years prior to 2008" in {
      // set up
      val contribution = Contribution(1914, 5000)

      // do it
      val isSupported = Pre2014Calculator.isSupported(contribution)

      // check it 
      isSupported shouldBe false
    }

    "not support calculations for tax years after 2013" in {
      // set up
      val contribution = Contribution(3090, 5000)

      // do it
      val isSupported = Pre2014Calculator.isSupported(contribution)

      // check it
      isSupported shouldBe false
    }

    "support calculations for tax years 2008 to 2013" in {
      val validContributions = for (taxYear <- Gen.choose(2008, 2013)) yield Contribution(taxYear, 5000)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2014 && contribution.taxPeriodStart.year > 2007) { 
          val result = Pre2014Calculator.isSupported(contribution) 
          result shouldBe true 
        }
      }
    }

    "return none for contributions prior to 2008" in {
      val invalidContributions = for (taxYear <- Gen.choose(Integer.MIN_VALUE, 2007)) yield Contribution(taxYear, 5000)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2008) { 
          val results = Pre2014Calculator.summary(Seq[SummaryResult](), contribution)
          results shouldBe None 
        }
      }
    }

    "return some calculation results for contributions between 2008 and 2013 inclusively" in {
      val validContributions = for (taxYear <- Gen.choose(2008, 2013);
                                    amount <- Gen.choose(0, Integer.MAX_VALUE)) 
                              yield Contribution(taxYear, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2014 && contribution.taxPeriodStart.year > 2007) { 
          val results = Pre2014Calculator.summary(Seq[SummaryResult](), contribution)
          results should not be None 
        }
      }
    }

    "return some for contributions prior to 2014 and after 2007" in new ZeroContributionFixture {
      // do it
      val results = Pre2014Calculator.summary(Seq[SummaryResult](), contribution)

      // check it
      results shouldBe Some(SummaryResult(-1,0,5000000,5000000,5000000,5000000,5000000))
    }

    "return amount exceeding Annual Allowance of 0 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.exceedingAAAmount shouldBe 0
    }

    "return amount chargable amount of -1 for values under 5000000 for tax years prior to 2011" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.chargableAmount shouldBe -1
    }

    "return amount chargable amount of 0 for values under 5000000 for tax years prior to 2011" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), Contribution(2013, 500000)).get

      // check it
      result.chargableAmount shouldBe 0
    }

    "return amount chargable amount of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](SummaryResult(),SummaryResult(),SummaryResult()), Contribution(2011, 5500000)).get

      // check it
      result.chargableAmount shouldBe 500000
    }

    "return amount exceeding Annual Allowance of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](SummaryResult(),SummaryResult(),SummaryResult()), Contribution(2011, 5500000)).get

      // check it
      result.exceedingAAAmount shouldBe 500000
    }

    "return available allowance of 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.availableAllowance shouldBe 5000000
    }

    "return unused allowance of 4400000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.unusedAllowance shouldBe 4400000
    }

    "return correct calculation results for contributions between 2008 and 2013 inclusively with no previous contributions" in {
      val validContributions = for (taxYear <- Gen.choose(2008, 2013);
                                    amount <- Gen.choose(0, Integer.MAX_VALUE)) 
                               yield Contribution(taxYear, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2014 && contribution.taxPeriodStart.year > 2007) { 
          // set up
          val ty = contribution.taxPeriodStart.year
          val previous = List.tabulate(ty-2008)(n => SummaryResult(availableAllowance=5000000,unusedAllowance=5000000))

          // do test
          val results = Pre2014Calculator.summary(previous, contribution)

          // check results
          results should not be None 

          val summaryResult = results.get
          val definedBenefit = contribution.amounts.definedBenefit
          summaryResult.chargableAmount shouldBe (if (contribution.taxPeriodStart.year < 2011) -1 else (definedBenefit-20000000L).max(0))
          summaryResult.exceedingAAAmount shouldBe (definedBenefit - 5000000).max(0)
          summaryResult.availableAllowance shouldBe 5000000L
          summaryResult.unusedAllowance shouldBe (5000000L - definedBenefit).max(0)
          summaryResult.availableAAWithCF shouldBe (5000000L * (previous.size+1).min(4))
          summaryResult.availableAAWithCCF shouldBe ((5000000L * (previous.size+1).min(3)) - (definedBenefit-((definedBenefit - 5000000).max(0)))).max(0)
        }
      }
    }

    "return available allowance with carry forward of 95000" in new ContributionPre2014Fixture {
      // do it
      val starting = SummaryResult(chargableAmount = 0,
                                   exceedingAAAmount = 0,
                                   availableAllowance = 5000000,
                                   unusedAllowance = 4500000,
                                   availableAAWithCF = 5000000,
                                   availableAAWithCCF = 4500000)
      val result = Pre2014Calculator.summary(Seq[SummaryResult](starting), contribution1).get

      // check it
      result.chargableAmount shouldBe -1
      result.exceedingAAAmount shouldBe 0
      result.availableAllowance shouldBe 5000000
      result.unusedAllowance shouldBe 4400000
      result.availableAAWithCF shouldBe 9500000
      result.availableAAWithCCF shouldBe 8900000
    }

    "return none if defined benefit contribution amounts are negative" in {
      val invalidContributions = for (amount <- Gen.choose(Integer.MIN_VALUE, -1)) yield Contribution(2008, amount)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.amounts.definedBenefit < 0) { 
          Pre2014Calculator.summary(Seq[SummaryResult](), contribution) shouldBe None 
        }
      }
    }
  }
} 
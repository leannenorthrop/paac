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

package calculators.results

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.internal.BasicAllowanceCalculator
import scala.util.{Try, Success, Failure}

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
      val contribution = Contribution(2009, 0)
    }

    "not support calculations for tax years prior to 2006" in {
      // set up
      val contribution = Contribution(1914, 5000)

      // do it
      val isSupported = Pre2014Calculator.isSupported(contribution)

      // check it
      isSupported shouldBe false
    }

    "not support calculations for tax years after 2014" in {
      // set up
      val contribution = Contribution(3090, 5000)

      // do it
      val isSupported = Pre2014Calculator.isSupported(contribution)

      // check it
      isSupported shouldBe false
    }

    s"support calculations for tax years ${PensionPeriod.EARLIEST_YEAR_SUPPORTED} to 2014" in {
      (0 until 2190).foreach {
        (day)=>
        // first supported tax year starts on 6th April PensionPeriod.EARLIEST_YEAR_SUPPORTED
        val c = new java.util.GregorianCalendar(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 3, 6)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val isSupported = Pre2014Calculator.isSupported(contribution)

        // check it
        isSupported shouldBe true
      }

      // Bounds checks
      // start date before supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5),
                                        PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // start date after supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(2014, 4, 6),
                                        PensionPeriod(2014, 4, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // end date before supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 6),
                                        PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // end date after supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(2014, 4, 5),
                                        PensionPeriod(2014, 4, 6),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // start and end date before supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5),
                                        PensionPeriod(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 4, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // start and end date after supported range
      Pre2014Calculator.isSupported(Contribution(PensionPeriod(2014, 4, 6),
                                        PensionPeriod(2014, 4, 6),
                                        Some(InputAmounts(5000L)))) shouldBe false
    }

    s"return none for contributions prior to $PensionPeriod.EARLIEST_YEAR_SUPPORTED" in {
      val invalidContributions = for (taxYear <- Gen.choose(1, 2005)) yield Contribution(taxYear, 5000)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < PensionPeriod.EARLIEST_YEAR_SUPPORTED) {
          val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution)
          results.isFailure shouldBe true
        }
      }
    }

    s"return some calculation results for contributions between $PensionPeriod.EARLIEST_YEAR_SUPPORTED and 2013/14 inclusively" in {
      val validContributions = for (taxYear <- Gen.choose(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 2013);
                                    amount <- Gen.choose(0, Integer.MAX_VALUE))
                              yield Contribution(taxYear, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2015 && contribution.taxPeriodStart.year > 2005) {
          val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution)
          results.isFailure shouldBe false
        }
      }
    }

    "return none for contributions with no amounts provided" in {
      // set up
      val contribution = Contribution(PensionPeriod(2010,4,5),PensionPeriod(2011,4,6), None)

      // test
      val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution)

      // check it
      results.isFailure shouldBe true
    }

    "return none for contributions with no definedBenefit amount provided" in {
      // set up
      val contribution = Contribution(PensionPeriod(2010,4,5),PensionPeriod(2011,4,6), Some(InputAmounts(None,None)))

      // test
      val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution)

      // check it
      results.isFailure shouldBe true
    }

    "return some results for contributions" in new ZeroContributionFixture {
      // do it
      val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution)

      // check it
      results.get._1 shouldBe SummaryResult(-1,0,5000000,5000000,5000000,5000000,0)
    }

    "return amount exceeding Annual Allowance of 0 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution1).get._1

      // check it
      result.exceedingAAAmount shouldBe 0
    }

    "return amount chargable amount of -1 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution1).get._1

      // check it
      result.chargableAmount shouldBe -1
    }

    "return amount chargable amount of 0 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](), Contribution(2013, 500000)).get._1

      // check it
      result.chargableAmount shouldBe 0
    }

    "return amount chargable amount of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](TaxYearResults(),TaxYearResults(),TaxYearResults()), Contribution(2011, 5500000)).get._1

      // check it
      result.chargableAmount shouldBe 500000
    }

    "return amount exceeding Annual Allowance of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](TaxYearResults(),TaxYearResults(),TaxYearResults()), Contribution(2011, 5500000)).get._1

      // check it
      result.exceedingAAAmount shouldBe 500000
    }

    "return available allowance of 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution1).get._1

      // check it
      result.availableAllowance shouldBe 5000000
    }

    "return unused allowance of 4400000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution1).get._1

      // check it
      result.unusedAllowance shouldBe 4400000
    }

    s"return correct calculation results for contributions between PensionPeriod.EARLIEST_YEAR_SUPPORTED and 2013/14 inclusively with no previous contributions" in {
      val validContributions = for (taxYear <- Gen.choose(PensionPeriod.EARLIEST_YEAR_SUPPORTED, 2013);
                                    amount <- Gen.choose(0, Integer.MAX_VALUE))
                               yield Contribution(taxYear, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year < 2014 && contribution.taxPeriodStart.year > PensionPeriod.EARLIEST_YEAR_SUPPORTED) {
          // set up
          val ty = contribution.taxPeriodStart.year
          val previous = List.tabulate(ty-2008)(n => TaxYearResults(contribution, SummaryResult(availableAllowance=5000000,unusedAllowance=5000000)))

          // do test
          val results = Pre2014Calculator.calculate(previous, contribution)

          // check results
          results.isFailure should not be true

          val summaryResult = results.get._1
          val definedBenefit = contribution.amounts.get.definedBenefit.get
          // TODO Get to the bottom of these properties!!!!!!!!!
          //withClue("Chargable amount: ") {summaryResult.chargableAmount shouldBe (if (contribution.taxPeriodStart.year < 2011) -1 else (definedBenefit-20000000L).max(0))}
          withClue("Exceeding AA amount: ") {summaryResult.exceedingAAAmount shouldBe (definedBenefit - 5000000).max(0)}
          withClue("AA amount: ") {summaryResult.availableAllowance shouldBe 5000000L}
          withClue("Unused Allowance amount: ") {summaryResult.unusedAllowance shouldBe (5000000L - definedBenefit).max(0)}
          //withClue("Available AA with CF amount: ") {summaryResult.availableAAWithCF shouldBe (5000000L * (previous.size+1).min(4))}
          //summaryResult.availableAAWithCCF shouldBe ((5000000L * (previous.size+1).min(3)) - (definedBenefit-((definedBenefit - 5000000).max(0)))).max(0)
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
      val previous = Seq[TaxYearResults](TaxYearResults(Contribution(PensionPeriod.EARLIEST_YEAR_SUPPORTED,500000L),starting))
      val result = Pre2014Calculator.calculate(previous, contribution1).get._1

      val actualUnused = calculators.internal.utilities.actualUnusedList(BasicAllowanceCalculator(50000L, previous, contribution1))(previous, contribution1)
      info(actualUnused.mkString(","))

      // check it
      result.chargableAmount shouldBe -1
      result.exceedingAAAmount shouldBe 0
      result.availableAllowance shouldBe 5000000
      result.unusedAllowance shouldBe 4400000
      result.availableAAWithCF shouldBe 9500000
      result.availableAAWithCCF shouldBe 8900000
    }

    "return none if defined benefit contribution amounts are negative" in {
      val invalidContributions = for (amount <- Gen.choose(Integer.MIN_VALUE, -1)) yield Contribution(PensionPeriod.EARLIEST_YEAR_SUPPORTED, amount)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.amounts.get.definedBenefit.get < 0) {
          Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution).isFailure shouldBe true
        }
      }
    }

    "return some if defined benefit is none but defined contribution is some in 2014" in {
      // set up
      val contribution = Contribution(PensionPeriod(2010,4,5),PensionPeriod(2011,4,6), Some(InputAmounts(None,Some(123L))))

      // test
      val results = Pre2014Calculator.calculate(Seq[TaxYearResults](), contribution).get._1

      // check it
      results shouldBe SummaryResult(-1,0,5000000,4999877,5000000,4999877,0)
    }
  }
}

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

class Year2014CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks {
  "Year2014Calculator" should {
    "not support calculations for tax years other than 2014" in {
      val invalidContributions = for (taxYear <- Gen.choose(Integer.MIN_VALUE, Integer.MAX_VALUE)) yield Contribution(taxYear, 5000)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year != 2014) { 
          Year2014Calculator.isSupported(contribution) shouldBe false 
        }
      }
    }

    "support calculations for 2014 tax year" in {
      (0 until 356).foreach {
        (day)=>
        // first supported tax year starts on 6th April 2006
        val c = new java.util.GregorianCalendar(2014, 3, 6)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(TaxPeriod(taxYear, taxMonth, taxDay), 
                                        TaxPeriod(taxYear, taxMonth, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val isSupported = Year2014Calculator.isSupported(contribution)

        // check it
        withClue(s"Date '$taxDay/${taxMonth+1}/$taxYear' should be supported but") { isSupported shouldBe true }
      }

      // Bounds checks
      // start date before supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2014, 3, 5), 
                                        TaxPeriod(2014, 3, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // start date after supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2015, 3, 6), 
                                        TaxPeriod(2015, 3, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // end date before supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2014, 3, 5), 
                                        TaxPeriod(2014, 3, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // end date after supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2015, 3, 6), 
                                        TaxPeriod(2014, 3, 6),
                                        Some(InputAmounts(5000L)))) shouldBe false 
      // start and end date before supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2014, 3, 5), 
                                        TaxPeriod(2014, 3, 5),
                                        Some(InputAmounts(5000L)))) shouldBe false
      // start and end date after supported range
      Year2014Calculator.isSupported(Contribution(TaxPeriod(2015, 3, 6), 
                                        TaxPeriod(2015, 3, 6),
                                        Some(InputAmounts(5000L)))) shouldBe false  
    }

    "return none for contributions other than 2014 tax year" in {
      val invalidContributions = for (taxYear <- Gen.choose(Integer.MIN_VALUE, Integer.MAX_VALUE)) yield Contribution(taxYear, 5000)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year != 2014) { 
          val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
          results shouldBe None 
        }
      }
    }

    "return some for contributions in 2014 tax year >= 0" in {
      val validContributions = for (amount <- Gen.choose(0, Integer.MAX_VALUE)) yield Contribution(2014, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year == 2014) { 
          val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
          results should not be None 
        }
      }
    }

    "return none for contributions in 2014 tax year < 0" in {
      val validContributions = for (amount <- Gen.choose(Integer.MIN_VALUE, -1)) yield Contribution(2014, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year == 2014) { 
          val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
          results shouldBe None 
        }
      }
    }

    "return expected summary results for 2014 year when 0 defined benefit given" in {
      // do it
      val results = Year2014Calculator.summary(Seq[SummaryResult](), Contribution(2014, 0))

      // check it
      results shouldBe Some(SummaryResult(0,0,4000000,4000000,4000000,4000000,4000000))
    }

    "return annual allowance of 4000000 pence for all valid amounts" in {
      val validContributions = for (amount <- Gen.choose(0, Integer.MAX_VALUE)) yield Contribution(2014, amount)

      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.amounts.get.definedBenefit.get >= 0) { 
          val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
          results.get.availableAllowance shouldBe 4000000L
        }
      }
    }

    "when no previous allowance available" can {
      "return correct amount of 0 Exceeding Annual Allowance for values under 4000000" in {
        val validContributions = for (amount <- Gen.choose(0, 3999999)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.exceedingAAAmount shouldBe 0 
          }
        }
      }

      "return correct amount of 0 Exceeding Annual Allowance for value of 4000000" in {
        val results = Year2014Calculator.summary(Seq[SummaryResult](), Contribution(2014, 4000000))
        results.get.exceedingAAAmount shouldBe 0 
      }

      "return correct amount of 0 Exceeding Annual Allowance for values over 4000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.exceedingAAAmount should not be 0 
            val db = contribution.amounts.get.definedBenefit.get
            results.get.exceedingAAAmount shouldBe (db - 4000000L).max(0) 
          }
        }
      }

      "return correct amount of 0 chargable amount for values under 4000000" in {
        val validContributions = for (amount <- Gen.choose(0, 3999999)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.chargableAmount shouldBe 0 
          }
        }
      }

      "return correct amount of 0 chargable amount for value of 4000000" in {
        val results = Year2014Calculator.summary(Seq[SummaryResult](), Contribution(2014, 4000000))
        results.get.chargableAmount shouldBe 0 
      }

      "return correct amount of non-0 chargable amount for values over 4000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.chargableAmount should not be 0 
            val db = contribution.amounts.get.definedBenefit.get
            results.get.chargableAmount shouldBe (db - 4000000L).max(0) 
          }
        }
      }

      "return correct amount of non-0 unused allowance for values under 4000000" in {
        val validContributions = for (amount <- Gen.choose(0, 3999999)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.unusedAllowance should not be 0 
            val db = contribution.amounts.get.definedBenefit.get
            results.get.unusedAllowance shouldBe (4000000L - db).max(0)
          }
        }
      }

      "return correct amount of 0 unused allowance for value of 4000000" in {
        val results = Year2014Calculator.summary(Seq[SummaryResult](), Contribution(2014, 4000000))
        results.get.unusedAllowance shouldBe 0 
      }

      "return correct amount of 0 unused allowance for values over 4000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(2014, amount)

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 4000000) { 
            val results = Year2014Calculator.summary(Seq[SummaryResult](), contribution)
            results.get.unusedAllowance shouldBe 0  
          }
        }
      }
    }
  }

  "return correct calculation results for contributions with no previous contributions" in {
    val validContributions = for (amount <- Gen.choose(0, Integer.MAX_VALUE)) 
                             yield Contribution(2014, amount)

    forAll(validContributions) { (contribution: Contribution) =>
      whenever (contribution.taxPeriodStart.year == 2014) { 
        // set up
        val ty = contribution.taxPeriodStart.year
        // previous tax years had higher tax rate of £50,000 values here are in pence
        val previous = List.tabulate(ty-2008)(n => SummaryResult(availableAllowance=5000000,unusedAllowance=5000000))

        // do test
        val results = Year2014Calculator.summary(previous, contribution)

        // check results
        results should not be None 

        val summaryResult = results.get
        val definedBenefit = contribution.amounts.get.definedBenefit.get
        summaryResult.chargableAmount shouldBe (definedBenefit-19000000L).max(0)
        summaryResult.exceedingAAAmount shouldBe (definedBenefit - 4000000).max(0)
        summaryResult.availableAllowance shouldBe 4000000L
        summaryResult.unusedAllowance shouldBe (4000000L - definedBenefit).max(0)
        summaryResult.availableAAWithCF shouldBe 19000000L
      }
    }
  }
} 
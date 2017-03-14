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

/* Year2015Period1Calculator and Year2015Period2Calculator were test driven from GroupXCalculationsSpec */
class Year2015Period2CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks {
  "Year 2015 Period 2 Calculator" should {
    "support defined benefits amounts for on 9th July but before 6th April 2016" in {
      (0 until 250).foreach {
        (day)=>
        // first supported tax year starts on 6th April 2006
        val c = new java.util.GregorianCalendar(2015, 6, 9)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val isSupported = Year2015Period2Calculator.isSupported(contribution)

        // check it
        withClue(s"Date '$taxDay/${taxMonth+1}/$taxYear' should be supported but") { isSupported shouldBe true }
      }
    }

    "not support defined benefits amounts for before 9th July 2015 or after 6th April 2016" in {
      var validContributions = for {taxYear <- Gen.choose(2015, 2015)
                                    taxMonth <- Gen.oneOf(1, 2, 3, 4, 5, 6)
                                    taxDay <- Gen.choose(1, 30)
                                    } yield Contribution(PensionPeriod(taxYear, taxMonth, taxDay),
                                                         PensionPeriod(taxYear, taxMonth, taxDay+1),
                                                         Some(InputAmounts(5000L)))

      // test excluded months
      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year == 2015) {
          Year2015Period2Calculator.isSupported(contribution) shouldBe false
        }
      }

      validContributions = for {taxYear <- Gen.choose(2016, 2016)
                                taxMonth <- Gen.oneOf(5, 6, 7, 8, 9, 10, 11, 12)
                                taxDay <- Gen.choose(1, 30)
                                } yield Contribution(PensionPeriod(taxYear, taxMonth, taxDay),
                                                     PensionPeriod(taxYear, taxMonth, taxDay+1),
                                                     Some(InputAmounts(5000L)))

      // test excluded months
      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year == 2016) {
          Year2015Period2Calculator.isSupported(contribution) shouldBe false
        }
      }

      // test bounds
      Year2015Period2Calculator.isSupported(Contribution(PensionPeriod(2015, 7, 8),
                                                  PensionPeriod(2016, 4, 5),
                                                  Some(InputAmounts(5000L)))) shouldBe false
      Year2015Period2Calculator.isSupported(Contribution(PensionPeriod(2015, 7, 9),
                                                  PensionPeriod(2015, 7, 10),
                                                  Some(InputAmounts(5000L)))) shouldBe true
      Year2015Period2Calculator.isSupported(Contribution(PensionPeriod(2016, 4, 5),
                                                  PensionPeriod(2016, 4, 5),
                                                  Some(InputAmounts(5000L)))) shouldBe true
      Year2015Period2Calculator.isSupported(Contribution(PensionPeriod(2016, 4, 6),
                                                  PensionPeriod(2016, 4, 6),
                                                  Some(InputAmounts(5000L)))) shouldBe false
    }

    "return none for contributions other than 2015 period 2" in {
      val invalidContributions = for {taxYear <- Gen.choose(1, Integer.MAX_VALUE)
                                      taxMonth <- Gen.choose(1, 12)
                                      taxDay <- Gen.choose(1, 30)} yield Contribution(PensionPeriod(taxYear, taxMonth, taxDay),
                                                                                      PensionPeriod(taxYear, taxMonth, taxDay+1),
                                                                                      Some(InputAmounts(5000L)))

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year != 2015 ||
                  contribution.taxPeriodStart.year != 2016) {
          val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), contribution)
          results shouldBe None
        }
      }
    }

    "return some results for contributions in 2015 period 2" in {
      (0 until 241).foreach {
        (day)=>
        // period 2 begins 9th July 2015
        val c = new java.util.GregorianCalendar(2015, 7, 9)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH) + 1
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth, taxDay),
                                        PensionPeriod(taxYear, taxMonth, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), contribution)

        // check it
        // With no previous inputs as period 2 has no allowance then exceeding is same as defined benefit input
        withClue(s"Contributions with date '$taxDay/${taxMonth}/$taxYear' should be supported but") { results shouldBe Some(ExtendedSummaryFields(5000,5000,0,0,0,0,0,0,1000000,0,0,0,0,0,0,5000,0,0,0,0,false,0,0,false)) }
      }
    }


    "when no previous allowance available" can {
      "return expected summary results when no previous entries supplied and 0 defined benefit is given" in {
        // do it
        val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), Contribution(false,0,0))
        // check it
        results shouldBe Some(ExtendedSummaryFields(0,0,0,0,0,0,0,0,1000000,0,0,0,0,0,0,0,0,0,0,0,false,0,0,false))
      }


      "return correct amount of 0 chargable amount for values under 4000000" in {
        val validContributions = for (amount <- Gen.choose(0, 3999999)) yield Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                           PensionPeriod.PERIOD_2_2015_END,
                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 4000000) {
            val resultsP1 = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END,Some(InputAmounts(0L)))).get
            val results = Year2015Period2Calculator.summary(Seq(TaxYearResults(contribution, resultsP1)), contribution)
            results.get.chargableAmount shouldBe 0
          }
        }
      }

      "return correct amount of 0 chargable amount for value of 4000000" in {
        val c = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END,Some(InputAmounts(0L)))
        val resultsP1 = Year2015Period1Calculator.summary(Seq[TaxYearResults](), c).get
        val results = Year2015Period2Calculator.summary(Seq(TaxYearResults(c, resultsP1)), Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                                        PensionPeriod.PERIOD_2_2015_END,
                                                                                                        Some(InputAmounts(4000000L))))
        results.get.chargableAmount shouldBe 0
      }

      "return correct amount of non-0 chargable amount for values over 4000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                                           PensionPeriod.PERIOD_2_2015_END,
                                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 4000000) {
            val resultsP1 = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END,Some(InputAmounts(4000000L)))).get
            val results = Year2015Period2Calculator.summary(Seq(TaxYearResults(contribution,resultsP1)), contribution)
            results.get.chargableAmount should not be 0
            val db = contribution.amounts.get.definedBenefit.get
            results.get.chargableAmount shouldBe (db - 4000000L).max(0)
          }
        }
      }

      "return correct amount of 0 unused allowance for value of 4000000" in {
        val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                           PensionPeriod.PERIOD_2_2015_END,
                                                                                           Some(InputAmounts(4000000L))))
        results.get.unusedAllowance shouldBe 0
      }

      "return correct amount of 0 unused allowance for values over 4000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                                           PensionPeriod.PERIOD_2_2015_END,
                                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 4000000) {
            val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.unusedAllowance shouldBe 0
          }
        }
      }

      "return None when defined benefit is None" in {
        // do it
        val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod.PERIOD_2_2015_START,
                                                                                           PensionPeriod.PERIOD_2_2015_END,
                                                                                           Some(InputAmounts(None, None, None))))
        // check it
        results shouldBe None
      }

      "moneyPurchaseAA" should {
        "return period 1 unused MPAA if triggered in period 1" in {
          // set up
          val p1 = Contribution(true, 0, 0).copy(amounts=Some(InputAmounts(Some(0),Some(0),None,Some(true))))
          val p2 = Contribution(false, 0, 0)
          val previous = TaxYearResults(p1, ExtendedSummaryFields(unusedMPAA=12345L))

          // test
          val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](previous), p2).get

          // check
          results.moneyPurchaseAA shouldBe 12345L
        }

        "return 10K if triggered in period 2" in {
          // set up
          val p1 = Contribution(true, 0, 0).copy(amounts=Some(InputAmounts(Some(0),Some(0),None,Some(false))))
          val p2 = Contribution(false, 0, 0).copy(amounts=Some(InputAmounts(Some(0),Some(0),None,Some(true))))
          val previous = TaxYearResults(p1, ExtendedSummaryFields(unusedMPAA=12345L))

          // test
          val results = Year2015Period2Calculator.summary(Seq[TaxYearResults](previous), p2).get

          // check
          results.moneyPurchaseAA shouldBe 1000000L
        }
      }
    }
  }
}

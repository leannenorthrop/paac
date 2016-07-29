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

package calculators.results

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

/* Year2015Period1Calculator and Year2015Period2Calculator were test driven from GroupXCalculationsSpec */

// scalastyle:off magic.number
// scalastyle:off line.size.limit
class Year2015Period1CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks {
  "Year 2015 Period 1 Calculator" should {
    "support defined benefits amounts for on 6 April but before 9th July 2015" in {
      (0 until 94).foreach {
        (day)=>
        // first supported tax year starts on 6th April 2006
        val c = new java.util.GregorianCalendar(2015, 3, 6)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth + 1, taxDay),
                                        PensionPeriod(taxYear, taxMonth + 1, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val isSupported = Year2015Period1Calculator.isSupported(contribution)

        // check it
        withClue(s"Date '$taxDay/${taxMonth + 1}/$taxYear' should be supported but") { isSupported shouldBe true }
      }
    }

    "not support defined benefits amounts for before 6 April or after 8th July 2015" in {
      // set up
      val validContributions = for {taxYear <- Gen.choose(2015, 2015)
                                    taxMonth <- Gen.oneOf(0, 1, 2, 7, 8, 9, 10, 11)
                                    taxDay <- Gen.choose(1, 30)
                                    } yield Contribution(PensionPeriod(taxYear, taxMonth + 1, taxDay),
                                                         PensionPeriod(taxYear, taxMonth + 1, taxDay + 1),
                                                         Some(InputAmounts(5000L)))

      // test excluded months
      forAll(validContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year == 2015) {
          Year2015Period1Calculator.isSupported(contribution) shouldBe false
        }
      }

      // test bounds
      Year2015Period1Calculator.isSupported(Contribution(PensionPeriod(2015, 4, 5),
                                                  PensionPeriod(2015, 4, 5),
                                                  Some(InputAmounts(5000L)))) shouldBe false
      Year2015Period1Calculator.isSupported(Contribution(PensionPeriod(2015, 4, 6),
                                                  PensionPeriod(2015, 4, 6),
                                                  Some(InputAmounts(5000L)))) shouldBe true
      Year2015Period1Calculator.isSupported(Contribution(PensionPeriod(2015, 7, 8),
                                                  PensionPeriod(2015, 7, 8),
                                                  Some(InputAmounts(5000L)))) shouldBe true
      Year2015Period1Calculator.isSupported(Contribution(PensionPeriod(2015, 7, 9),
                                                  PensionPeriod(2015, 7, 9),
                                                  Some(InputAmounts(5000L)))) shouldBe false
    }

    "return none for contributions other than 2015 period 1" in {
      val invalidContributions = for (taxYear <- Gen.choose(1, Integer.MAX_VALUE)) yield Contribution(taxYear, 5000)

      forAll(invalidContributions) { (contribution: Contribution) =>
        whenever (contribution.taxPeriodStart.year != 2015) {
          val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
          results shouldBe None
        }
      }
    }

    "return some results for contributions other than 2015 period 1" in {
      (0 until 94).foreach {
        (day)=>
        // first supported tax year starts on 6th April 2006
        val c = new java.util.GregorianCalendar(2015, 3, 6)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth + 1, taxDay),
                                        PensionPeriod(taxYear, taxMonth + 1, taxDay),
                                        Some(InputAmounts(5000L)))

        // do it
        val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)

        // check it
        withClue(s"Contributions with date '$taxDay/${taxMonth + 1}/$taxYear' should be supported but") { results shouldBe Some(ExtendedSummaryFields(0,0,8000000,4000000,8000000,4000000,0,0,2000000,0,0,0,0,0,0,5000,0,0,5000,0,false,5995000,0)) }
      }
    }

    "when no previous allowance available" can {
      "return expected summary results when no previous entries supplied and 0 defined benefit is given" in {
        // do it
        val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod(2015, 4, 7),
                                                                              PensionPeriod(2015, 4, 9),
                                                                              Some(InputAmounts(0L))))
        // check it
        results shouldBe Some(ExtendedSummaryFields(0,0,8000000,4000000,8000000,4000000,0,0,2000000,acaCF=6000000))
      }

      "return annual allowance of 8000000 pence for all valid amounts" in {
        val validContributions = for (amount <- Gen.choose(0, Integer.MAX_VALUE)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                                     PensionPeriod(2015, 4, 9),
                                                                                                     Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get >= 0) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.availableAllowance shouldBe 8000000L
          }
        }
      }

      "return correct amount of 0 Exceeding Annual Allowance for value of 8000000" in {
        val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod(2015, 4, 7),
                                                                                           PensionPeriod(2015, 4, 9),
                                                                                           Some(InputAmounts(8000000L))))
        results.get.exceedingAAAmount shouldBe 0
      }

      "return correct amount of 0 Exceeding Annual Allowance for values over 8000000" in {
        val validContributions = for (amount <- Gen.choose(8000001, Integer.MAX_VALUE)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                                           PensionPeriod(2015, 4, 9),
                                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 8000000) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.exceedingAAAmount should not be 0
            val db = contribution.amounts.get.definedBenefit.get
            results.get.exceedingAAAmount shouldBe (db - 8000000L).max(0)
          }
        }
      }

      "return correct amount of 0 chargable amount for values under 8000000" in {
        val validContributions = for (amount <- Gen.choose(0, 3999999)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                           PensionPeriod(2015, 4, 9),
                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 8000000) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.chargableAmount shouldBe 0
          }
        }
      }

      "return correct amount of 0 chargable amount for value of 8000000" in {
        val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod(2015, 4, 7),
                                                                                           PensionPeriod(2015, 4, 9),
                                                                                           Some(InputAmounts(8000000L))))
        results.get.chargableAmount shouldBe 0
      }

      "return correct amount of non-0 chargable amount for values over 8000000" in {
        val validContributions = for (amount <- Gen.choose(4000001, Integer.MAX_VALUE)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                                           PensionPeriod(2015, 4, 9),
                                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 8000000) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.chargableAmount should not be 0
            val db = contribution.amounts.get.definedBenefit.get
            results.get.chargableAmount shouldBe (db - 8000000L).max(0)
          }
        }
      }

      "return correct amount of non-0 unused allowance for values under 8000000" in {
        val validContributions = for (amount <- Gen.choose(0, 7999999)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                           PensionPeriod(2015, 4, 9),
                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get < 8000000) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.unusedAllowance should not be 0
            val db = contribution.amounts.get.definedBenefit.get
            if (8000000L - db > 4000000) {
              results.get.unusedAllowance shouldBe (if ((8000000L - db)>4000000) 4000000 else 8000000L - db).max(0)
            } else {
              results.get.unusedAllowance shouldBe (8000000L - db).max(0)
            }
          }
        }
      }

      "return correct amount of 0 unused allowance for value of 8000000" in {
        val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), Contribution(PensionPeriod(2015, 4, 7),
                                                                                           PensionPeriod(2015, 4, 9),
                                                                                           Some(InputAmounts(8000000L))))
        results.get.unusedAllowance shouldBe 0
      }

      "return correct amount of 0 unused allowance for values over 8000000" in {
        val validContributions = for (amount <- Gen.choose(8000001, Integer.MAX_VALUE)) yield Contribution(PensionPeriod(2015, 4, 7),
                                                                                                           PensionPeriod(2015, 4, 9),
                                                                                                           Some(InputAmounts(amount)))

        forAll(validContributions) { (contribution: Contribution) =>
          whenever (contribution.amounts.get.definedBenefit.get > 8000000) {
            val results = Year2015Period1Calculator.summary(Seq[TaxYearResults](), contribution)
            results.get.unusedAllowance shouldBe 0
          }
        }
      }
    }

    "unused allowance" should {
      "when mpa not applicable and aca applies return pre-trigger amount" in {
        // set up
        val test = new calculators.internal.Year2015Period1Calculator() {
          def allowanceInPounds(): Long = 8000000L
          def previousPeriods(): Seq[TaxYearResults] = Seq[TaxYearResults](TaxYearResults(Contribution(2015,4200000L), ExtendedSummaryFields()))
          def contribution(): Contribution = Contribution(2015, Some(InputAmounts(triggered=Some(true))))
          override def isTriggered(): Boolean = true
          override def isMPAAApplicable(): Boolean = false
          override def defaultChargableAmount(): Long = 10L
          override def alternativeChargableAmount(): Long = 20L
        }

        // test
        val results = test.unusedAllowance()

        // check
        results shouldBe 3800000L
      }
    }
  }
}
// scalastyle:on magic.number
// scalastyle:on line.size.limit

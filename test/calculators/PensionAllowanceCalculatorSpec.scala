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

import play.api.Play
import uk.gov.hmrc.play.test.UnitSpec
import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeApplication}
import models._

class PensionAllowanceCalculatorSpec extends UnitSpec with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop()
  }

  "PensionAllowanceCalculator" should {
    "return 0 for any negative defined benefit" in {
      // set up
      val input = Contribution(2010, -9000L)
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult(0,0,0,0,0,0,0))
    }

    "return 0 for contributions prior to 2006" in {
      // set up
      val input = Contribution(1914, 100)
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult(0,0,0,0,0,0,0))
    }

    "provideMissingYearContributions" should {
      object Test extends PensionAllowanceCalculator {
        def test(contributions : Seq[Contribution]): List[Contribution] = {
          provideMissingYearContributions(contributions)
        }
      }

      "create period 1 contribution of 0 when period 2 supplied" in {
        // set up
        val period2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(5000L)))

        // test
        val results = Test.test(Seq(Contribution(2014, 500000L),period2))

        // check
        results.size shouldBe 9
        results.find(_.taxYearLabel == "2015/16 P1").get shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(0L),Some(0L))))
      }

      "return contributions in year order if given values out of order" in {
        // set up
        val contributions = Seq(Contribution(2014,0L),Contribution(2012,0L),Contribution(2011,0L))

        // test
        val results = Test.test(contributions)

        // check
        Some(Seq(Contribution(2011,0L),Contribution(2012,0L),Contribution(2014,0L),Contribution(2014,0L)))
      }

      "return interim period values if not provided" in {
        // set up
        val contributions = Seq(Contribution(2011,0L),Contribution(2014,0L))

        // test
        val results = Test.test(contributions)

        // check
        Some(Seq(Contribution(2011,0L),Contribution(2012,0L),Contribution(2014,0L),Contribution(2014,0L)))
      }

      "return corrected sorted contributions" in {
        // set up
        val c1 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(false))))
        val c2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L), Some(2L), None, Some(true))))
        val contributions = Seq(c2, c1)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        info(r.mkString("\n"))
        r(0) shouldBe c2
        r(1) shouldBe c1
      }

      "will provide missing pre-trigger p1 contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod(2015,6,1),PensionPeriod(2015,7,8),Some(InputAmounts(Some(0),Some(1000000),None,Some(true))))
        val contributions = Seq(c1)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(1) shouldBe c1
        r(2) shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,6,1),Some(InputAmounts(Some(0),Some(0),None,Some(false))))
      }

      "will provide missing pre-trigger p1 contribution if 2 contributions triggered" in {
        // set up
        val c0 = Contribution(PensionPeriod.PERIOD_1_2015_START,PensionPeriod.PERIOD_1_2015_END,Some(InputAmounts(Some(0),Some(1000000),None,Some(true))))
        val c1 = Contribution(PensionPeriod.PERIOD_2_2015_START,PensionPeriod.PERIOD_2_2015_END,Some(InputAmounts(Some(0),Some(1000000),None,Some(true))))
        val contributions = Seq(c1, c0)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(0) shouldBe c1
        r(1) shouldBe c0
        r(2) shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,4,6),Some(InputAmounts(Some(0),Some(0),None,Some(false))))
      }

      "will provide missing pre-trigger p2 contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod.PERIOD_1_2015_START,PensionPeriod.PERIOD_1_2015_END,Some(InputAmounts(Some(0),Some(1000000),None,Some(false))))
        val c2 = Contribution(PensionPeriod.PERIOD_2_2015_START,PensionPeriod.PERIOD_2_2015_END,Some(InputAmounts(Some(0),Some(1000000),None,Some(true))))
        val contributions = Seq(c1,c2)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(0) shouldBe c2
        r(1) shouldBe Contribution(PensionPeriod(2015,7,9),PensionPeriod(2015,7,9),Some(InputAmounts(Some(0),Some(0),None,Some(false)))) 
        r(2) shouldBe c1
      }
    }

    "collate" should {
      object AllowanceCalculator extends PensionAllowanceCalculator {}

      "not collapse when no trigger row result" in {
        // set up
        val result0 = TaxYearResults(Contribution(PensionPeriod(2012,4,6),PensionPeriod(2013,4,5),Some(InputAmounts(Some(2000000),Some(2000000),None,Some(false)))),SummaryResult(0,0,5000000,1000000,20000000,11000000,0))
        val result1 = TaxYearResults(Contribution(PensionPeriod(2013,4,6),PensionPeriod(2014,4,5),Some(InputAmounts(Some(2000000),Some(2000000),None,Some(false)))),SummaryResult(0,0,5000000,1000000,16000000,7000000,0))
        val result2 = TaxYearResults(Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(Some(2000000),Some(2000000),None,Some(false)))),SummaryResult(0,0,4000000,0,11000000,2000000,0))
        val result3 = TaxYearResults(Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(0),Some(0),None,None))),ExtendedSummaryFields(0,0,8000000,4000000,10000000,6000000,0,0,0,2000000,0,0,0,0,0,0,0,0,0,0,0,false,8000000,0))
        val result4 = TaxYearResults(Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(2900000),Some(1100000),None,None))),ExtendedSummaryFields(0,0,4000000,0,6000000,1000000,0,0,0,0,1100000,1100000,0,1100000,0,100000,0,0,0,0,0,true,0,0))

        // test
        val collatedResults = PensionAllowanceCalculator.collate(Seq(result0, result1, result2, result3, result4))

        // check
        collatedResults.size shouldBe 5
        collatedResults(0) shouldBe result0
        collatedResults(1) shouldBe result1
        collatedResults(2) shouldBe result2
        collatedResults(3) shouldBe result3
        collatedResults(4) shouldBe result4
      }

      "collapse period 1 trigger row result" in {
        // set up
        val result0 = TaxYearResults(Contribution(PensionPeriod(2012,4,6),PensionPeriod(2013,4,5),Some(InputAmounts(Some(4000000),Some(500000),None,Some(false)))),SummaryResult(0,0,5000000,500000,20000000,10500000,0))
        val result1 = TaxYearResults(Contribution(PensionPeriod(2013,4,6),PensionPeriod(2014,4,5),Some(InputAmounts(Some(4000000),Some(700000),None,Some(false)))),SummaryResult(0,0,5000000,300000,15500000,5800000,0))
        val result2 = TaxYearResults(Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(Some(3000000),Some(200000),None,Some(false)))),SummaryResult(0,0,4000000,800000,9800000,1600000,0))
        val result3 = TaxYearResults(Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(8000000),Some(500000),None,Some(false)))),ExtendedSummaryFields(0,500000,8000000,0,9600000,1100000,0,0,0,1600000,500000,0,0,500000,8500000,0,0,0,0,9000000,0,false,-1400000,0))
        val result4 = TaxYearResults(Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(0),Some(800000),None,Some(true)))),ExtendedSummaryFields(0,1300000,8000000,0,9600000,300000,0,0,0,0,800000,0,0,800000,8500000,0,0,0,1000000,8500000,9300000,false,0,300000))
        val result5 = TaxYearResults(Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(4700000),Some(600000),None,Some(true)))),ExtendedSummaryFields(5000000,0,0,0,300000,0,0,1000000,0,4400000,0,0,5000000,1400000,13200000,0,0,0,0,8500000,5300000,false,0,0))

        // test
        val collatedResults = PensionAllowanceCalculator.collate(Seq(result0, result1, result2, result3, result4, result5))

        // check
        collatedResults.size shouldBe 5
        collatedResults(0) shouldBe result0
        collatedResults(1) shouldBe result1
        collatedResults(2) shouldBe result2
        collatedResults(3) shouldBe TaxYearResults(input=result3.input,summaryResult=result4.summaryResult)
        collatedResults(4) shouldBe result5
      }

      "collapse period 2 trigger row result" in {
        // set up
        val result0 = TaxYearResults(Contribution(PensionPeriod(2012,4,6),PensionPeriod(2013,4,5),Some(InputAmounts(Some(5000000),Some(0),None,Some(false)))),SummaryResult(0,0,5000000,0,20000000,10000000,0))
        val result1 = TaxYearResults(Contribution(PensionPeriod(2013,4,6),PensionPeriod(2014,4,5),Some(InputAmounts(Some(5000000),Some(0),None,Some(false)))),SummaryResult(0,0,5000000,0,15000000,5000000,0))
        val result2 = TaxYearResults(Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(Some(4000000),Some(0),None,Some(false)))),SummaryResult(0,0,4000000,0,9000000,0,0))
        val result3 = TaxYearResults(Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(10000000),Some(1500000),None,Some(false)))),ExtendedSummaryFields(3500000,3500000,8000000,0,8000000,0,0,0,0,4000000,1500000,0,0,1500000,11500000,0,0,0,0,13000000,0,false,-7000000,0))
        val result4 = TaxYearResults(Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(1500000),Some(1200000),None,Some(false)))),ExtendedSummaryFields(2700000,2700000,0,0,0,0,0,0,0,0,1200000,1200000,1200000,2700000,11500000,200000,0,0,0,0,0,true,0,0))
        val result5 = TaxYearResults(Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(0),Some(2500000),None,Some(true)))),ExtendedSummaryFields(5200000,0,0,0,0,0,0,0,0,2700000,1500000,4200000,5200000,4000000,13000000,1500000,0,0,0,2700000,4000000,true,0,0))

        // test
        val collatedResults = PensionAllowanceCalculator.collate(Seq(result0, result1, result2, result3, result4, result5))

        // check
        collatedResults.size shouldBe 5
        collatedResults(0) shouldBe result0
        collatedResults(1) shouldBe result1
        collatedResults(2) shouldBe result2
        collatedResults(3) shouldBe result3
        collatedResults(4) shouldBe TaxYearResults(input=result4.input,summaryResult=result5.summaryResult)
      }
    }
  }

  // Calculation based tests are found in logic.XXCalculationsSpec
} 
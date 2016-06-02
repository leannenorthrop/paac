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
    }
  }

  // Calculation based tests are found in logic.XXCalculationsSpec
} 
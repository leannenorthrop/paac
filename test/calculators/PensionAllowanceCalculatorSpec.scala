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
        def expandedTest(contributions : Seq[Contribution],
                         earliestYear: Int,
                         treatMissingRowsAsRegisteredPensionYears: Boolean): List[Contribution] = {
          provideMissingYearContributions(contributions,earliestYear,treatMissingRowsAsRegisteredPensionYears)
        }
      }

      "create period 1 contribution of 0 when period 2 supplied" in {
        // set up
        val period2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(5000L)))

        // test
        val results = Test.test(Seq(Contribution(2014, 500000L),period2))

        // check
        results.size shouldBe 9
        results.find(_.taxYearLabel == "2015/16 P1").get shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),0,0,false)
      }

      "return missing early contributions if not supplied" in {
        // set up
        val contributions = Seq(Contribution(2014,0L),Contribution(2012,0L))

        // test
        val results = Test.expandedTest(contributions, 2009, true)

        // check
        results shouldBe List(Contribution(2009,0L),Contribution(2010,0L),Contribution(2011,0L),Contribution(2012,0L),Contribution(2013,0L),Contribution(2014,0L))
      }      

      "return maxed out contributions if not supplied and missingRowsAreRegistered is false" in {
        // set up
        val contributions = Seq(Contribution(2014,0L),Contribution(2012,0L))

        // test
        val results = Test.expandedTest(contributions, 2009, false)

        // check
        results shouldBe List(Contribution(PensionPeriod(2009,4,6),PensionPeriod(2010,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2010,4,6),PensionPeriod(2011,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2011,4,6),PensionPeriod(2012,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2012,4,6),PensionPeriod(2013,4,5),Some(InputAmounts(Some(0),None,None,None))), 
                              Contribution(PensionPeriod(2013,4,6),PensionPeriod(2014,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(Some(0),None,None,None)))) 
      }

      "return maxed out period 1 and period 2 contributions if not supplied and missingRowsAreRegistered is false" in {
        // set up
        val contributions = Seq(Contribution(2013,123L),Contribution(2016,246L))

        // test
        val results = Test.expandedTest(contributions, 2009, false)

        // check
        results shouldBe List(Contribution(PensionPeriod(2009,4,6),PensionPeriod(2010,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2010,4,6),PensionPeriod(2011,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2011,4,6),PensionPeriod(2012,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2012,4,6),PensionPeriod(2013,4,5),Some(InputAmounts(Some(5000000),None,None,None))), 
                              Contribution(PensionPeriod(2013,4,6),PensionPeriod(2014,4,5),Some(InputAmounts(Some(123),None,None,None))), 
                              Contribution(PensionPeriod(2014,4,6),PensionPeriod(2015,4,5),Some(InputAmounts(Some(4000000),None,None,None))), 
                              Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(8000000),Some(0),None,Some(false)))), 
                              Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(0),Some(0),None,Some(false)))), 
                              Contribution(PensionPeriod(2016,4,6),PensionPeriod(2017,4,5),Some(InputAmounts(Some(246),None,None,None))))
      }

      "return contributions in year order if given values out of order" in {
        // set up
        val contributions = Seq(Contribution(2014,0L),Contribution(2012,0L),Contribution(2011,0L))

        // test
        val results = Test.expandedTest(contributions, 2010, true)

        // check
        results shouldBe List(Contribution(2010,0L),Contribution(2011,0L),Contribution(2012,0L),Contribution(2013,0L),Contribution(2014,0L))
      }

      "return interim period values if not provided" in {
        // set up
        val contributions = Seq(Contribution(2011,0L),Contribution(2014,0L))

        // test
        val results = Test.expandedTest(contributions, 2010, true)

        // check
        results shouldBe List(Contribution(2010,0L),Contribution(2011,0L),Contribution(2012,0L),Contribution(2013,0L),Contribution(2014,0L))
      }

      "return corrected sorted contributions" in {
        // set up
        val c1 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, 1L, 2L, false)
        val c2 = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, 1L, 2L, true)
        val contributions = Seq(c2, c1)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        //info(r.mkString("\n"))
        r(0) shouldBe c2
        r(1) shouldBe c1
      }

      "will provide missing pre-trigger p1 contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod(2015,6,1),PensionPeriod(2015,7,8),0,1000000,true)
        val contributions = Seq(c1)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(1) shouldBe c1
        r(2) shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,6,1),0,0,false)
      }

      "will provide missing pre-trigger p1 contribution if 2 contributions triggered" in {
        // set up
        val c0 = Contribution(PensionPeriod.PERIOD_1_2015_START,PensionPeriod.PERIOD_1_2015_END,1000000,0,true)
        val c1 = Contribution(PensionPeriod.PERIOD_2_2015_START,PensionPeriod.PERIOD_2_2015_END,1000000,0,true)
        val contributions = Seq(c1, c0)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(0) shouldBe c1
        r(1) shouldBe c0
        r(2) shouldBe Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,4,6),0,0,false)
      }

      "will provide missing pre-trigger p2 contribution" in {
        // set up
        val c1 = Contribution(PensionPeriod.PERIOD_1_2015_START,PensionPeriod.PERIOD_1_2015_END,0,1000000,false)
        val c2 = Contribution(PensionPeriod.PERIOD_2_2015_START,PensionPeriod.PERIOD_2_2015_END,0,1000000,true)
        val contributions = Seq(c1,c2)

        // test
        val results = Test.test(contributions)

        // check
        val r = results.reverse
        r(0) shouldBe c2
        r(1) shouldBe Contribution(PensionPeriod(2015,7,9),PensionPeriod(2015,7,9),0,0,false)
        r(2) shouldBe c1
      }
    }

    "collate" should {
      trait TestFixture {
        def pre2015Contribution(year: Int, db: Long, dc: Long, triggered: Boolean): Contribution = {
          Contribution(PensionPeriod(year,4,6),PensionPeriod(year+1,4,5),Some(InputAmounts(Some(db),Some(dc),None,if (!triggered) None else Some(true))))
        }
        def p1Contribution(db: Long, dc: Long, triggered: Boolean): Contribution = {
          Contribution(PensionPeriod(2015,4,6),PensionPeriod(2015,7,8),Some(InputAmounts(Some(db),Some(dc),None,if (!triggered) None else Some(true))))
        }
        def p2Contribution(db: Long, dc: Long, triggered: Boolean): Contribution = {
          Contribution(PensionPeriod(2015,7,9),PensionPeriod(2016,4,5),Some(InputAmounts(Some(db),Some(dc),None,if (!triggered) None else Some(true))))
        }
      }

      trait P2 {
        val sr1 = SummaryResult(0,0,5000000,0,20000000,10000000,0)
        val sr2 = SummaryResult(0,0,5000000,0,15000000,5000000,0)
        val sr3 = SummaryResult(0,0,4000000,0,9000000,0,0)
        val sr4 = ExtendedSummaryFields(3500000,3500000,8000000,0,8000000,0,0,0,0,0,4000000,1500000,0,0,1500000,11500000,0,0,13000000,0,false,-7000000,0)
        val sr5 = ExtendedSummaryFields(2700000,2700000,0,0,0,0,0,0,0,0,0,1200000,1200000,1200000,2700000,11500000,200000,0,0,0,true,0,0)
        val sr6 = ExtendedSummaryFields(5200000,0,0,0,0,0,0,0,0,0,2700000,1500000,4200000,5200000,4000000,13000000,1500000,0,2700000,4000000,true,0,0)
      }

      object AllowanceCalculator extends PensionAllowanceCalculator {}

      "not collapse when no trigger row result" in new TestFixture {
        // set up
        val result0 = TaxYearResults(pre2015Contribution(2012, 2000000, 2000000, false),SummaryResult(0,0,5000000,1000000,20000000,11000000,0))
        val result1 = TaxYearResults(pre2015Contribution(2013, 2000000, 2000000, false),SummaryResult(0,0,5000000,1000000,16000000,7000000,0))
        val result2 = TaxYearResults(pre2015Contribution(2014, 2000000, 2000000, false),SummaryResult(0,0,4000000,0,11000000,2000000,0))
        val result3 = TaxYearResults(p1Contribution(0,0,false),ExtendedSummaryFields(0,0,8000000,4000000,10000000,6000000,0, 0,0,0,2000000,0,0,0,0,0,0,0,0,0,false,8000000,0))
        val result4 = TaxYearResults(p2Contribution(2900000, 1100000, false),ExtendedSummaryFields(0,0,4000000,0,6000000,1000000,0,0,0,0,0,1100000,1100000,0,1100000,0,100000,0,0,0,true,0,0))

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

      "collapse period 1 trigger row result" in new TestFixture {
        // set up
        val result0 = TaxYearResults(pre2015Contribution(2012,4000000,500000,false),SummaryResult(0,0,5000000,500000,20000000,10500000,0))
        val result1 = TaxYearResults(pre2015Contribution(2013,4000000,700000,false),SummaryResult(0,0,5000000,300000,15500000,5800000,0))
        val result2 = TaxYearResults(pre2015Contribution(2014,3000000,200000,false),SummaryResult(0,0,4000000,800000,9800000,1600000,0))
        val result3 = TaxYearResults(p1Contribution(8000000,500000,false),ExtendedSummaryFields(0,500000,8000000,0,9600000,1100000,0,0,0,0,1600000,500000,0,0,500000,8500000,0,0,9000000,0,false,-1400000,0))
        val result4 = TaxYearResults(p1Contribution(0,800000,true),ExtendedSummaryFields(0,1300000,8000000,0,9600000,300000,0,1000000,0,0,0,800000,0,0,800000,8500000,0,0,8500000,9300000,false,0,300000))
        val result5 = TaxYearResults(p2Contribution(4700000,600000,true),ExtendedSummaryFields(5000000,0,0,0,300000,0,0,0,1000000,0,4400000,0,0,5000000,1400000,13200000,0,0,8500000,5300000,false,0,0))

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

      "collapse period 2 trigger row result" in new TestFixture with P2 {
        // set up
        val result0 = TaxYearResults(pre2015Contribution(2012,5000000,0,false),sr1)
        val result1 = TaxYearResults(pre2015Contribution(2013,5000000,0,false),sr2)
        val result2 = TaxYearResults(pre2015Contribution(2014,4000000,0,false),sr3)
        val result3 = TaxYearResults(p1Contribution(10000000,1500000,false),sr4)
        val result4 = TaxYearResults(p2Contribution(1500000, 1200000,false),sr5)
        val result5 = TaxYearResults(p2Contribution(0,2500000,true),sr6)

        // test
        val collatedResults = PensionAllowanceCalculator.collate(Seq(result0, result1, result2, result3, result4, result5))

        // check
        collatedResults.size shouldBe 5
        collatedResults(0) shouldBe result0
        collatedResults(1) shouldBe result1
        collatedResults(2) shouldBe result2
        collatedResults(3) shouldBe result3
        collatedResults(4) shouldBe TaxYearResults(input=result4.input,summaryResult=result5.summaryResult.asInstanceOf[ExtendedSummaryFields].copy(chargableAmount=7900000))
      }

      "collapse period 2 trigger row result when trigger date is 5/4/2016" in new TestFixture with P2 {
        // set up
        val tdp = PensionPeriod(2016,4,5) 
        val result0 = TaxYearResults(pre2015Contribution(2012,5000000,0,false),sr1)
        val result1 = TaxYearResults(pre2015Contribution(2012,5000000,0,false),sr2)
        val result2 = TaxYearResults(pre2015Contribution(2012,4000000,0,false),sr3)
        val result3 = TaxYearResults(p1Contribution(10000000,1500000,false),sr4)
        val result4 = TaxYearResults(p2Contribution(1500000,1200000,false).copy(taxPeriodEnd=tdp),sr5)
        val result5 = TaxYearResults(p2Contribution(0,2500000,true).copy(taxPeriodEnd=tdp, taxPeriodStart=tdp),sr6)

        // test
        val collatedResults = PensionAllowanceCalculator.collate(Seq(result0, result1, result2, result3, result4, result5))

        // check
        collatedResults.size shouldBe 5
        collatedResults(0) shouldBe result0
        collatedResults(1) shouldBe result1
        collatedResults(2) shouldBe result2
        collatedResults(3) shouldBe result3
        collatedResults(4) shouldBe TaxYearResults(input=result4.input,summaryResult=result5.summaryResult.asInstanceOf[ExtendedSummaryFields].copy(chargableAmount=7900000))
      }
    }
  }

  // Calculation based tests are found in logic.XXCalculationsSpec
} 
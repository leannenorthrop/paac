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

import play.api.Play
import uk.gov.hmrc.play.test.UnitSpec
import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeApplication}
import models._

// Based on https://docs.google.com/spreadsheets/d/1W14DzxdyOIWHarnEf5p8FF3V0Pn84cSjmMLHW9C0oc8
class CalculationsSpec extends UnitSpec with BeforeAndAfterAll {
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

  "Group 1" should {
    "when defined benefit is 0 carry forwards and chargable amounts should be correct" in {
      // set up
      val c2008 = Contribution(2008, 0L)
      val c2009 = Contribution(2009, 0L)
      val c2010 = Contribution(2010, 0L)
      val c2011 = Contribution(2011, 0L)
      val c2012 = Contribution(2012, 0L)
      val c2013 = Contribution(2013, 0L)
      val c2014 = Contribution(2014, 0L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(0L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(0L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,10000000L,15000000L,20000000L,20000000L,20000000L,19000000L,22000000L,18000000L)
      // available allowance carried forward to following year
      val expectedAACCF = List(5000000L,10000000L,15000000L,15000000L,15000000L,15000000L,14000000L,18000000L,13000000L)
      // unused AA
      val expectedUnusedAA = List(5000000L,5000000L,5000000L,5000000L,5000000L,5000000L,4000000L,4000000L,4000000L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe expectedUnusedAA(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe expectedAACCF(i)}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe (if (result.input.taxPeriodStart.year < 2011) -1L else 0L)}
      }
    }

    "when defined benefit is non-0 carry forwards and chargable amounts should be correct" in {
      // set up
      val c2008 = Contribution(2008, 500000L)
      val c2009 = Contribution(2009, 600000L)
      val c2010 = Contribution(2010, 700000L)
      val c2011 = Contribution(2011, 800000L)
      val c2012 = Contribution(2012, 900000L)
      val c2013 = Contribution(2013, 1000000L)
      val c2014 = Contribution(2014, 1100000L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(1200000L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(1300000L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,9500000L,13900000L,18200000L,17900000L,17600000L,16300000L,19000000L,15000000L)
      // available allowance carried forward to following year
      val expectedAACCF = List(4500000L,8900000L,13200000L,12900000L,12600000L,12300000L,11000000L,13800000L,9600000L)
      // unused AA
      val expectedUnusedAA = List(4500000L,4400000L,4300000L,4200000L,4100000L,4000000L,2900000L,4000000L,2700000L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe expectedUnusedAA(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe expectedAACCF(i)}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe (if (result.input.taxPeriodStart.year < 2011) -1L else 0L)}
      }
    }

    "when defined benefit is equal to allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val c2008 = Contribution(2008, 5000000L)
      val c2009 = Contribution(2009, 5000000L)
      val c2010 = Contribution(2010, 5000000L)
      val c2011 = Contribution(2011, 5000000L)
      val c2012 = Contribution(2012, 5000000L)
      val c2013 = Contribution(2013, 5000000L)
      val c2014 = Contribution(2014, 4000000L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(8000000L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(0L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,5000000L,5000000L,5000000L,5000000L,5000000L,4000000L,8000000L,4000000L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe 0L}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe 0L}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe (if (result.input.taxPeriodStart.year < 2011) -1L else 0L)}
      }
    }

    "when defined benefit is equal to allowances carry forwards and chargable amounts should be correct2" in {
      // set up
      val c2008 = Contribution(2008, 5000000L)
      val c2009 = Contribution(2009, 5000000L)
      val c2010 = Contribution(2010, 5000000L)
      val c2011 = Contribution(2011, 5000000L)
      val c2012 = Contribution(2012, 5000000L)
      val c2013 = Contribution(2013, 5000000L)
      val c2014 = Contribution(2014, 4000000L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(4000000L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(4000000L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,5000000L,5000000L,5000000L,5000000L,5000000L,4000000L,8000000L,4000000L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe (if (result.input.isPeriod1()) 4000000L else 0L)}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe (if (result.input.isPeriod1()) 4000000L else 0L)}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe (if (result.input.taxPeriodStart.year < 2011) -1L else 0L)}
      }
    }

    "when defined benefit is above annual allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val c2008 = Contribution(2008, 5100000L)
      val c2009 = Contribution(2009, 5100000L)
      val c2010 = Contribution(2010, 5100000L)
      val c2011 = Contribution(2011, 5100000L)
      val c2012 = Contribution(2012, 5100000L)
      val c2013 = Contribution(2013, 5100000L)
      val c2014 = Contribution(2014, 4100000L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(8100000L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(4100000L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,5000000L,5000000L,5000000L,5000000L,5000000L,4000000L,8000000L,4000000L)
      // chargable
      val expectedCharge = List(-1L,-1L,-1L,100000L,100000L,100000L,100000L,100000L,4100000L)
      val expectedExceeding = List(-1L,-1L,-1L,100000L,100000L,100000L,100000L,100000L,4100000L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe 0L}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe 0L}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe expectedCharge(i)}
          withClue (s"${result.input.taxYearLabel} exceeding"){ result.summaryResult.chargableAmount shouldBe expectedExceeding(i)}
      }
    }

    "when defined benefit is either below, same or above annual allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val c2008 = Contribution(2008, 9000000L)
      val c2009 = Contribution(2009, 3000000L)
      val c2010 = Contribution(2010, 2100000L)
      val c2011 = Contribution(2011, 5000000L)
      val c2012 = Contribution(2012, 4500000L)
      val c2013 = Contribution(2013, 2000000L)
      val c2014 = Contribution(2014, 3200000L)
      val c2015p1 = Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(6500000L,0L)))
      val c2015p2 = Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(2010000L,0L)))
      val inputs = List(c2008, c2009, c2010, c2011, c2012, c2013, c2014, c2015p1, c2015p2)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      // total available allowance for current year
      val expectedAACF = List(5000000L,5000000L,7000000L,9900000L,9900000L,8400000L,7500000L,12300000L,5800000L)
      // available allowance carried forward to following year
      val expectedAACCF = List(0L,2000000L,4900000L,4900000L,3400000L,3500000L,4300000L,5300000L,3290000L)
      // chargable
      val expectedCharge = List(-1L,-1L,-1L,0L,0L,0L,0L,0L,0L)
      val expectedExceeding = List(-1L,-1L,-1L,0L,0L,0L,0L,0L,0L)
      val expectedUnused = List(0L,2000000L,2900000,0L,500000L,3000000L,800000L,1500000L,0L)
      results.zipWithIndex.foreach {
        case (result,i)=>
          withClue (s"${result.input.taxYearLabel} unused allowance"){ result.summaryResult.unusedAllowance shouldBe expectedUnused(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CF "){ result.summaryResult.availableAAWithCF shouldBe expectedAACF(i)}
          withClue (s"${result.input.taxYearLabel} available AA with CCF"){ result.summaryResult.availableAAWithCCF shouldBe expectedAACCF(i)}
          withClue (s"${result.input.taxYearLabel} chargable amount"){ result.summaryResult.chargableAmount shouldBe expectedCharge(i)}
          withClue (s"${result.input.taxYearLabel} exceeding"){ result.summaryResult.chargableAmount shouldBe expectedExceeding(i)}
      }
    }
  }
}

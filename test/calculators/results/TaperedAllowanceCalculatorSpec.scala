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
import play.api.Play
import org.scalatest.BeforeAndAfterAll
import play.api.test.{FakeApplication}
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.SummaryResultCalculator

class TaperedAllowanceCalculatorSpec extends UnitSpec with BeforeAndAfterAll {
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

  "isTaperingApplicable" should {
    class Test(contribution:Contribution) extends TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution) {
      def test(): Boolean = super.isTaperingApplicable
    }

    "should return true when adjusted income is over 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe true
    }

    "should return false when adjusted income is = 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000000))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe false
    }

    "should return false when adjusted income is < 150K" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(14900000))))

      // test
      val result = new Test(contribution).test

      // check
      result shouldBe false
    }
  }

  "Tapered allowance" should {
    "should be 40K when tapering is not applicable" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(14900000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be 40K when tapering is applicable and adjusted income is £150,001" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000100))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 4000000L
    }

    "should be £39,999 when tapering is applicable and adjusted income is £150,002" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(15000200))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 3999900L
    }

    "should be £15k when tapering is applicable and adjusted income is £200,000" in {
      // set up
      val contribution = Contribution(2016, Some(InputAmounts(income=Some(20000000))))

      // test
      val result = TaperedAllowanceCalculator()(Seq[TaxYearResults](), contribution).annualAllowance

      // check
      result shouldBe 1500000L
    }
  }
}

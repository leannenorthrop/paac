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

class Post2015CalculatorSpec extends UnitSpec with BeforeAndAfterAll {
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

  "Year 2016 / Post 2015 Calculator" should {
    trait ZeroContributionFixture {
      val contribution = Contribution(2016, 0)
    }

    "not support calculations for tax years prior to 2016" in {
      // set up
      val contribution = Contribution(1915, 5000)

      // do it
      val isSupported = Post2015Calculator.isSupported(contribution)

      // check it 
      isSupported shouldBe false
    }

    "not support calculations for tax years after 2100" in {
      // set up
      val contribution = Contribution(2100, 5000)

      // do it
      val isSupported = Post2015Calculator.isSupported(contribution)

      // check it
      isSupported shouldBe false
    }

    s"support calculations for tax years 2016 to 2100" in {
      (0 until 30650).foreach {
        (day)=>
        val c = new java.util.GregorianCalendar(2016, 4, 6)
        c.add(java.util.Calendar.DAY_OF_MONTH,day)
        val taxYear = c.get(java.util.Calendar.YEAR)
        val taxMonth = c.get(java.util.Calendar.MONTH)
        val taxDay = c.get(java.util.Calendar.DAY_OF_MONTH)

        val contribution = Contribution(PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        PensionPeriod(taxYear, taxMonth+1, taxDay),
                                        Some(InputAmounts(5000L)))
        // do it
        val isSupported = Post2015Calculator.isSupported(contribution)

        // check it
        isSupported shouldBe true
      }
    }

    "return all 0 values in results" in {
      // set up
      val contribution = Contribution(2016, 0)

      // test
      val result = Post2015Calculator.summary(Seq[TaxYearResults](), contribution)
      
      // check
      result should not be None
      result.get shouldBe ExtendedSummaryFields(0,0,4000000,0,0,0,0,0,1000000,3000000,0,0,0,0,0,0,0,0,0,0,false,0,0)
    }

    "annual allowance" in {
      // set up
      val contribution = Contribution(2016, 0)

      // test
      val result = Post2015Calculator.summary(Seq[TaxYearResults](), contribution)

      // check
      result.get.availableAllowance shouldBe 4000000L
    }
  }
}

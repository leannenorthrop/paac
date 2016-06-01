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

package calculators.periods

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import calculators.results.BasicCalculator

class Group1P2CalculatorSpec extends UnitSpec {
  trait TestFixture {
    val annualAllowance = 50000L
    implicit val amountsCalculator = BasicCalculator(annualAllowance)
  }

  "Group1P2Calculator" should {
    "use basic calculator" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2014, 123L)
      val expectedBasicSummary = amountsCalculator.summary.get

      // check
      Group1P2Calculator().annualAllowance shouldBe annualAllowance * 100L
      Group1P2Calculator().definedBenefit shouldBe amountsCalculator.definedBenefit
    }

    "aaCCF" should {
      "when given no previous results (not realistic usage) it should return 0" in new TestFixture {
        // set up
        implicit val previousPeriods = List[TaxYearResults]()
        implicit val contribution = Contribution(2015, 123L)

        // check
        Group1P2Calculator().aaCCF shouldBe 0L
      }
    }
  }
}

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

import play.api.Play
import play.api.test.FakeApplication
import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import calculators.results.BasicCalculator

class Group1P1CalculatorSpec extends UnitSpec with GeneratorDrivenPropertyChecks with BeforeAndAfterAll {
  trait TestFixture {
    val annualAllowance = 50000
    val basicCalculator = BasicCalculator(annualAllowance)
    val group1P1Calculator = Group1P1Calculator(basicCalculator)
  }

  "Group1P1Calculator" should {
    "use basic calculator" in new TestFixture {
      // set up
      implicit val previousPeriods = List[TaxYearResults]()
      implicit val contribution = Contribution(2014, 123L)
      val expectedBasicSummary = basicCalculator.summary

      // do
      val maybeSummary = group1P1Calculator.summary

      // check
      maybeSummary should not be None
      maybeSummary.get.unusedAllowanceCF shouldBe 0L
      maybeSummary.get.chargableAmount shouldBe maybeSummary.get.chargableAmount
      maybeSummary.get.exceedingAAAmount shouldBe maybeSummary.get.exceedingAAAmount
      maybeSummary.get.availableAllowance shouldBe maybeSummary.get.availableAllowance
      maybeSummary.get.unusedAllowance shouldBe maybeSummary.get.unusedAllowance
    }
  }
}

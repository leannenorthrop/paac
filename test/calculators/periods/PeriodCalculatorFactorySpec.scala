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

class PeriodCalculatorFactorySpec extends UnitSpec {
  trait TestFixture {
    val allowanceInPounds = 12L
    implicit val amountsCalculator = calculators.results.BasicCalculator(allowanceInPounds)
    implicit val previousPeriods = List[TaxYearResults]()
  }

  "PeriodCalculatorFactory" should {
    "return Group1P1Calculator for period 1 group 1 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, None)
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group1P1Calculator()
    }

    "return Group2P1Calculator for period 1 group 2 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L),Some(2L))))
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group2P1Calculator()
    }

    "return Group2P1Calculator for period 1 group 3 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(1L),Some(2L),None,Some(true))))
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group2P1Calculator()
    }

    "return Group1P2Calculator for period 2 group 1 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, None)
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group1P2Calculator()
    }

    "return Group2P2Calculator for period 2 group 2 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L),Some(2L))))
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group2P2Calculator()
    }

    "return Group3P2Calculator for period 2 group 3 contribution" in new TestFixture {
      implicit val contribution = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(1L),Some(2L),None,Some(true))))
      PeriodCalculatorFactory.get(allowanceInPounds).get shouldBe Group3P2Calculator()
    }

    "return None for unknown" in new TestFixture {
      implicit val contribution = Contribution(2012,0)
      PeriodCalculatorFactory.get(allowanceInPounds) shouldBe None
    }
  }
}

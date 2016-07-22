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

import uk.gov.hmrc.play.test.UnitSpec
import models._
import org.scalatest._
import calculators.results._

class CalculatorSpec extends UnitSpec {
  "Calculator" should {
    "create 0 allowance calculator when contribution is not supported by standard calculators" in {
      // set up
      val contribution = Contribution(1900, 0)

      // test
      val result = Calculator(contribution)

      // check
      result.allowance(contribution) shouldBe 0L
      result.summary(Seq[TaxYearResults](), contribution) shouldBe None
    }

    "create pre 2014" in {
      // set up
      val contribution = Contribution(2013, 0)

      // test
      val result = Calculator(contribution)

      // check
      result shouldBe Pre2014Calculator
    }
    "create 2014" in {
      // set up
      val contribution = Contribution(2014, 0)

      // test
      val result = Calculator(contribution)

      // check
      result shouldBe Year2014Calculator
    }
    "create 2015 period 1" in {
      // set up
      val contribution = Contribution(true, 0, 0)

      // test
      val result = Calculator(contribution)

      // check
      result shouldBe Year2015Period1Calculator
    }
    "create 2015 period 2" in {
      // set up
      val contribution = Contribution(false, 0, 0)

      // test
      val result = Calculator(contribution)

      // check
      result shouldBe Year2015Period2Calculator
    }
    "create 2016" in {
      // set up
      val contribution = Contribution(2016, 0)

      // test
      val result = Calculator(contribution)

      // check
      result shouldBe Post2015Calculator
    }
  }
}

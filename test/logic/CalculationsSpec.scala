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
      val inputs = Utilties.generateContributions(Map("2008"->0L,
                                                      "2009"->0L,
                                                      "2010"->0L,
                                                      "2011"->0L,
                                                      "2012"->0L,
                                                      "2013"->0L,
                                                      "2014"->0L,
                                                      "2015P1"->0L,
                                                      "2015P2"->0L))

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 0                   | -1               | 50000                      | 50000        | 50000
                     :2009   | 0                   | -1               | 100000                     | 50000        | 100000
                     :2010   | 0                   | -1               | 150000                     | 50000        | 150000
                     :2011   | 0                   | 0                | 200000                     | 50000        | 150000
                     :2012   | 0                   | 0                | 200000                     | 50000        | 150000
                     :2013   | 0                   | 0                | 200000                     | 50000        | 150000
                     :2014   | 0                   | 0                | 190000                     | 40000        | 140000
                     :2015P1 | 0                   | 0                | 220000                     | 40000        | 180000
                     :2015P2 | 0                   | 0                | 180000                     | 40000        | 130000
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }

    "when defined benefit is non-0 carry forwards and chargable amounts should be correct" in {
      // set up
      val inputs = Utilties.generateContributions(Map("2008"->500000L,
                                                      "2009"->600000L,
                                                      "2010"->700000L,
                                                      "2011"->800000L,
                                                      "2012"->900000L,
                                                      "2013"->1000000L,
                                                      "2014"->1100000L,
                                                      "2015P1"->1200000L,
                                                      "2015P2"->1300000L))
      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 0                   | -1               | 50000                      | 45000        | 45000
                     :2009   | 0                   | -1               | 95000                      | 44000        | 89000
                     :2010   | 0                   | -1               | 139000                     | 43000        | 132000
                     :2011   | 0                   | 0                | 182000                     | 42000        | 129000
                     :2012   | 0                   | 0                | 179000                     | 41000        | 126000
                     :2013   | 0                   | 0                | 176000                     | 40000        | 123000
                     :2014   | 0                   | 0                | 163000                     | 29000        | 110000
                     :2015P1 | 0                   | 0                | 190000                     | 40000        | 138000
                     :2015P2 | 0                   | 0                | 150000                     | 27000        | 96000
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }

    "when defined benefit is equal to allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val inputs = Utilties.generateContributions(Map("2008"->5000000L,
                                                      "2009"->5000000L,
                                                      "2010"->5000000L,
                                                      "2011"->5000000L,
                                                      "2012"->5000000L,
                                                      "2013"->5000000L,
                                                      "2014"->4000000L,
                                                      "2015P1"->8000000L,
                                                      "2015P2"->0L))

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 0                   | -1               | 50000                      | 0            | 0
                     :2009   | 0                   | -1               | 50000                      | 0            | 0
                     :2010   | 0                   | -1               | 50000                      | 0            | 0
                     :2011   | 0                   | 0                | 50000                      | 0            | 0
                     :2012   | 0                   | 0                | 50000                      | 0            | 0
                     :2013   | 0                   | 0                | 50000                      | 0            | 0
                     :2014   | 0                   | 0                | 40000                      | 0            | 0
                     :2015P1 | 0                   | 0                | 80000                      | 0            | 0
                     :2015P2 | 0                   | 0                | 40000                      | 0            | 0
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }

    "when defined benefit is equal to allowances to 2015 carry forwards and chargable amounts should be correct" in {
      // set up
      val inputs = Utilties.generateContributions(Map("2008"->5000000L,
                                                      "2009"->5000000L,
                                                      "2010"->5000000L,
                                                      "2011"->5000000L,
                                                      "2012"->5000000L,
                                                      "2013"->5000000L,
                                                      "2014"->4000000L,
                                                      "2015P1"->4000000L,
                                                      "2015P2"->4000000L))

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 0                   | -1               | 50000                      | 0            | 0
                     :2009   | 0                   | -1               | 50000                      | 0            | 0
                     :2010   | 0                   | -1               | 50000                      | 0            | 0
                     :2011   | 0                   | 0                | 50000                      | 0            | 0
                     :2012   | 0                   | 0                | 50000                      | 0            | 0
                     :2013   | 0                   | 0                | 50000                      | 0            | 0
                     :2014   | 0                   | 0                | 40000                      | 0            | 0
                     :2015P1 | 0                   | 0                | 80000                      | 40000        | 40000
                     :2015P2 | 0                   | 0                | 40000                      | 0            | 0
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }

    "when defined benefit is above annual allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val inputs = Utilties.generateContributions(Map("2008"->5100000L,
                                                      "2009"->5100000L,
                                                      "2010"->5100000L,
                                                      "2011"->5100000L,
                                                      "2012"->5100000L,
                                                      "2013"->5100000L,
                                                      "2014"->4100000L,
                                                      "2015P1"->8100000L,
                                                      "2015P2"->4100000L))

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 1000                | -1               | 50000                      | 0            | 0
                     :2009   | 1000                | -1               | 50000                      | 0            | 0
                     :2010   | 1000                | -1               | 50000                      | 0            | 0
                     :2011   | 1000                | 1000             | 50000                      | 0            | 0
                     :2012   | 1000                | 1000             | 50000                      | 0            | 0
                     :2013   | 1000                | 1000             | 50000                      | 0            | 0
                     :2014   | 1000                | 1000             | 40000                      | 0            | 0
                     :2015P1 | 1000                | 1000             | 80000                      | 0            | 0
                     :2015P2 | 41000               | 41000            | 40000                      | 0            | 0
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }

    "when defined benefit is either below, same or above annual allowances carry forwards and chargable amounts should be correct" in {
      // set up
      val inputs = Utilties.generateContributions(Map("2008"->9000000L,
                                                      "2009"->3000000L,
                                                      "2010"->2100000L,
                                                      "2011"->5000000L,
                                                      "2012"->4500000L,
                                                      "2013"->2000000L,
                                                      "2014"->3200000L,
                                                      "2015P1"->6500000L,
                                                      "2015P2"->2010000L))

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 9
      val table = """:year   | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2008   | 40000               | -1               | 50000                      | 0            | 0
                     :2009   | 0                   | -1               | 50000                      | 20000        | 20000
                     :2010   | 0                   | -1               | 70000                      | 29000        | 49000
                     :2011   | 0                   | 0                | 99000                      | 0            | 49000
                     :2012   | 0                   | 0                | 99000                      | 5000         | 34000
                     :2013   | 0                   | 0                | 84000                      | 30000        | 35000
                     :2014   | 0                   | 0                | 75000                      | 8000         | 43000
                     :2015P1 | 0                   | 0                | 123000                     | 15000        | 53000
                     :2015P2 | 0                   | 0                | 58000                      | 0            | 32900
                     :""".stripMargin(':')
      Utilties.assertResults(table, results)
    }
  }
}

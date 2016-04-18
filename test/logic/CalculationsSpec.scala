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

  def doTest(inputs: Map[String,Long], expectedValuesTable: String): Unit = {
    val results = PensionAllowanceCalculator.calculateAllowances(Utilties.generateContributions(inputs))
    Utilties.assertResults(expectedValuesTable, results)
  }

  def doTest(table: String): Unit = {
    val years = table.split('\n').drop(1).toList.map(_.split('|').toList(0).trim)
    val definedBenefit = table.split('\n').drop(1).toList.map(_.split('|').toList(1).trim.toLong)
    val inputs = Map(years.zip(definedBenefit): _*)
    val results = PensionAllowanceCalculator.calculateAllowances(Utilties.generateContributions(inputs))
    Utilties.assertResults(table, results)
  }

  "Group 1" should {

    "Pre-2014" should {
      "return correct calculations when pension contributions are 0" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 0               | 0                   | -1               | 150000                     | 50000        | 150000
                       :2009   | 0               | 0                   | -1               | 200000                     | 50000        | 150000
                       :2010   | 0               | 0                   | -1               | 200000                     | 50000        | 150000
                       :2011   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2012   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2013   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct calculations when pension contributions are 50000" in {
        // check it
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 50000           | 0                   | -1               | 150000                     | 0            | 100000
                       :2009   | 50000           | 0                   | -1               | 150000                     | 0            | 50000
                       :2010   | 50000           | 0                   | -1               | 100000                     | 0            | 0 
                       :2011   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :2012   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :2013   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct calculations when pension contributions are 40000" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 40000           | 0                   | -1               | 150000                     | 10000        | 110000
                       :2009   | 40000           | 0                   | -1               | 160000                     | 10000        | 70000 
                       :2010   | 40000           | 0                   | -1               | 120000                     | 10000        | 30000 
                       :2011   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :2012   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :2013   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct calculations when pension contributions are 60000" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 60000           | 10000               | -1               | 150000                     | 0            | 100000
                       :2009   | 60000           | 10000               | -1               | 150000                     | 0            | 50000
                       :2010   | 60000           | 10000               | -1               | 100000                     | 0            | 0
                       :2011   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :2012   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :2013   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct calculations when pension contributions are variable amounts above and below allowance" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 0               | 0                   | -1               | 150000                     | 50000        | 150000
                       :2009   | 50000           | 0                   | -1               | 200000                     | 0            | 100000
                       :2010   | 60000           | 10000               | -1               | 150000                     | 0            | 50000
                       :2011   | 150000          | 100000              | 50000            | 100000                     | 0            | 0
                       :2012   | 40000           | 0                   | 0                | 50000                      | 10000        | 10000
                       :2013   | 50000           | 0                   | 0                | 60000                      | 0            | 10000
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct allowances and carry forward values" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 5000            | 0                   | -1               | 150000                     | 45000        | 145000
                       :2009   | 6000            | 0                   | -1               | 195000                     | 44000        | 139000
                       :2010   | 7000            | 0                   | -1               | 189000                     | 43000        | 132000
                       :2011   | 8000            | 0                   | 0                | 182000                     | 42000        | 129000
                       :2012   | 9000            | 0                   | 0                | 179000                     | 41000        | 126000
                       :2013   | 10000           | 0                   | 0                | 176000                     | 40000        | 123000
                       :""".stripMargin(':')
        doTest(table)
      }
    }

    "2014 Calculations" should {
      "return correct allowances and carry forward values" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 5000            | 0                   | -1               | 150000                     | 45000        | 145000
                       :2009   | 6000            | 0                   | -1               | 195000                     | 44000        | 139000
                       :2010   | 7000            | 0                   | -1               | 189000                     | 43000        | 132000
                       :2011   | 8000            | 0                   | 0                | 182000                     | 42000        | 129000
                       :2012   | 9000            | 0                   | 0                | 179000                     | 41000        | 126000
                       :2013   | 10000           | 0                   | 0                | 176000                     | 40000        | 123000
                       :2014   | 11000           | 0                   | 0                | 163000                     | 29000        | 110000
                       :""".stripMargin(':')
        doTest(table)
      }
    }

    "2015 Period 1" should {
      "when defined benefit is 0 carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 5000            | 0                   | -1               | 150000                     | 45000        | 145000
                       :2009   | 6000            | 0                   | -1               | 195000                     | 44000        | 139000
                       :2010   | 7000            | 0                   | -1               | 189000                     | 43000        | 132000
                       :2011   | 8000            | 0                   | 0                | 182000                     | 42000        | 129000
                       :2012   | 9000            | 0                   | 0                | 179000                     | 41000        | 126000
                       :2013   | 10000           | 0                   | 0                | 176000                     | 40000        | 123000
                       :2014   | 11000           | 0                   | 0                | 163000                     | 29000        | 110000
                       :2015P1 | 12000           | 0                   | 0                | 190000                     | 40000        | 150000
                       :""".stripMargin(':')
        doTest(table)
      }
    }

    "2015 Period 2" should {
      "when defined benefit is 0 carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 0               | 0                   | -1               | 150000                     | 50000        | 150000
                       :2009   | 0               | 0                   | -1               | 200000                     | 50000        | 150000
                       :2010   | 0               | 0                   | -1               | 200000                     | 50000        | 150000
                       :2011   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2012   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2013   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2014   | 0               | 0                   | 0                | 190000                     | 40000        | 140000
                       :2015P1 | 0               | 0                   | 0                | 220000                     | 40000        | 180000
                       :2015P2 | 0               | 0                   | 0                | 180000                     | 40000        | 130000
                       :""".stripMargin(':')
        doTest(table)
      }

      "when defined benefit is non-0 carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 5000            | 0                   | -1               | 150000                     | 45000        | 145000
                       :2009   | 6000            | 0                   | -1               | 195000                     | 44000        | 139000
                       :2010   | 7000            | 0                   | -1               | 189000                     | 43000        | 132000
                       :2011   | 8000            | 0                   | 0                | 182000                     | 42000        | 129000
                       :2012   | 9000            | 0                   | 0                | 179000                     | 41000        | 126000
                       :2013   | 10000           | 0                   | 0                | 176000                     | 40000        | 123000
                       :2014   | 11000           | 0                   | 0                | 163000                     | 29000        | 110000
                       :2015P1 | 12000           | 0                   | 0                | 190000                     | 40000        | 150000
                       :2015P2 | 13000           | 0                   | 0                | 150000                     | 27000        | 96000
                       :""".stripMargin(':')
        doTest(table)
      }

      "when defined benefit is equal to allowances carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 50000           | 0                   | -1               | 150000                     | 0            | 100000
                       :2009   | 50000           | 0                   | -1               | 150000                     | 0            | 50000
                       :2010   | 50000           | 0                   | -1               | 100000                     | 0            | 0
                       :2011   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2012   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2013   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2014   | 40000           | 0                   | 0                | 40000                      | 0            | 0
                       :2015P1 | 80000           | 0                   | 0                | 80000                      | 0            | 0
                       :2015P2 | 0               | 0                   | 0                | 0                          | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "when defined benefit is equal to allowances to 2015 carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 50000           | 0                   | -1               | 150000                     | 0            | 100000
                       :2009   | 50000           | 0                   | -1               | 150000                     | 0            | 50000
                       :2010   | 50000           | 0                   | -1               | 100000                     | 0            | 0
                       :2011   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2012   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2013   | 50000           | 0                   | 0                | 50000                      | 0            | 0
                       :2014   | 40000           | 0                   | 0                | 40000                      | 0            | 0
                       :2015P1 | 40000           | 0                   | 0                | 80000                      | 40000        | 40000
                       :2015P2 | 40000           | 0                   | 0                | 40000                      | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "when defined benefit is above annual allowances carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 51000           | 1000                | -1               | 150000                     | 0            | 100000
                       :2009   | 51000           | 1000                | -1               | 150000                     | 0            | 50000
                       :2010   | 51000           | 1000                | -1               | 100000                     | 0            | 0
                       :2011   | 51000           | 1000                | 1000             | 50000                      | 0            | 0
                       :2012   | 51000           | 1000                | 1000             | 50000                      | 0            | 0
                       :2013   | 51000           | 1000                | 1000             | 50000                      | 0            | 0
                       :2014   | 41000           | 1000                | 1000             | 40000                      | 0            | 0
                       :2015P1 | 81000           | 1000                | 1000             | 80000                      | 0            | 0
                       :2015P2 | 41000           | 41000               | 41000            | 0                          | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "when defined benefit is either below, same or above annual allowances carry forwards and chargable amounts should be correct" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 90000           | 40000               | -1               | 150000                     | 0            | 100000
                       :2009   | 30000           | 0                   | -1               | 150000                     | 20000        | 70000
                       :2010   | 21000           | 0                   | -1               | 120000                     | 29000        | 49000
                       :2011   | 50000           | 0                   | 0                | 99000                      | 0            | 49000
                       :2012   | 45000           | 0                   | 0                | 99000                      | 5000         | 34000
                       :2013   | 20000           | 0                   | 0                | 84000                      | 30000        | 35000
                       :2014   | 32000           | 0                   | 0                | 75000                      | 8000         | 43000
                       :2015P1 | 65000           | 0                   | 0                | 123000                     | 15000        | 58000
                       :2015P2 | 20100           | 5100                | 0                | 58000                      | 0            | 38000
                       :""".stripMargin(':')
        doTest(table)
      }

      "when all previous allowances used and input is 7000000 for period 2 2015 should calculate correct amounts" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2013   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                       :2014   | 40000           | 0                   | 0                | 140000                     | 0            | 50000
                       :2015P1 | 80000           | 0                   | 0                | 130000                     | 0            | 50000
                       :2015P2 | 70000           | 70000               | 20000            | 50000                      | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "when input is 40000 for period 2 2015 should calculate correct amounts" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2014   | 25000           | 0                   | 0                | 190000                     | 15000        | 115000
                       :2015P1 | 80000           | 0                   | 0                | 195000                     | 0            | 115000
                       :2015P2 | 40000           | 40000               | 0                | 115000                     | 0            | 65000
                       :""".stripMargin(':')
        doTest(table)
      }
    }
  }

  "Scenario Group 1 P1 0 P2 75k" should {
    "when defined benefit Period 1 is 0 return expected results" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 45000           | 0                   | 0                | 200000                     | 5000         | 105000
                     :2013   | 50000           | 0                   | 0                | 155000                     | 0            | 55000
                     :2014   | 30000           | 0                   | 0                | 95000                      | 10000        | 15000
                     :2015P1 | 0               | 0                   | 0                | 95000                      | 40000        | 55000
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 0 and Period 2 is 75000 return expected results" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 45000           | 0                   | 0                | 200000                     | 5000         | 105000
                     :2013   | 50000           | 0                   | 0                | 155000                     | 0            | 55000
                     :2014   | 30000           | 0                   | 0                | 95000                      | 10000        | 15000
                     :2015P1 | 0               | 0                   | 0                | 95000                      | 40000        | 55000
                     :2015P2 | 75000           | 35000               | 20000            | 55000                      | 0            | 10000
                     :""".stripMargin(':')
      doTest(table)
    }
  }
}

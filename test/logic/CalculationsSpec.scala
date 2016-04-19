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

  def doTest(table: String, print: Boolean = false): Unit = {
    val years = table.split('\n').drop(1).toList.map(_.split('|').toList(0).trim)
    val definedBenefit = table.split('\n').drop(1).toList.map(_.split('|').toList(1).trim.toLong)
    val inputs = Map(years.zip(definedBenefit): _*)
    val results = PensionAllowanceCalculator.calculateAllowances(Utilties.generateContributions(inputs))
    Utilties.assertResults(table, results, print)
  }
  
  "Group 1" should {
    "pre-2014" should {
      "return correct calculations when pension contributions are 0" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 0               | 0                   | -1               | 50000                      | 50000        | 50000
                       :2009   | 0               | 0                   | -1               | 100000                     | 50000        | 100000
                       :2010   | 0               | 0                   | -1               | 150000                     | 50000        | 150000
                       :2011   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2012   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :2013   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                       :""".stripMargin(':')
        doTest(table)
      } 

      "return correct calculations when pension contributions are 50000" in {
        // check it
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2009   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2010   | 50000           | 0                   | -1               | 50000                      | 0            | 0 
                       :2011   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :2012   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :2013   | 50000           | 0                   | 0                | 50000                      | 0            | 0 
                       :""".stripMargin(':')
        doTest(table)
      } 
      "return correct calculations when pension contributions are 40000" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 40000           | 0                   | -1               | 50000                      | 10000        | 10000
                       :2009   | 40000           | 0                   | -1               | 60000                      | 10000        | 20000 
                       :2010   | 40000           | 0                   | -1               | 70000                      | 10000        | 30000 
                       :2011   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :2012   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :2013   | 40000           | 0                   | 0                | 80000                      | 10000        | 30000 
                       :""".stripMargin(':')
        doTest(table)
      }


      "return correct calculations when pension contributions are 60000" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 60000           | 10000               | -1               | 50000                      | 0            | 0
                       :2009   | 60000           | 10000               | -1               | 50000                      | 0            | 0
                       :2010   | 60000           | 10000               | -1               | 50000                      | 0            | 0
                       :2011   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :2012   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :2013   | 60000           | 10000               | 10000            | 50000                      | 0            | 0
                       :""".stripMargin(':')
        doTest(table)
      }

      "return correct calculations when pension contributions are variable amounts above and below allowance" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 0               | 0                   | -1               | 50000                      | 50000        | 50000
                       :2009   | 50000           | 0                   | -1               | 100000                     | 0            | 50000
                       :2010   | 60000           | 10000               | -1               | 100000                     | 0            | 50000
                       :2011   | 150000          | 100000              | 50000            | 100000                     | 0            | 0
                       :2012   | 40000           | 0                   | 0                | 50000                      | 10000        | 10000
                       :2013   | 50000           | 0                   | 0                | 60000                      | 0            | 10000
                       :""".stripMargin(':')
        doTest(table)
      } 

      "return correct allowances and carry forward values" in {
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 5000            | 0                   | -1               | 50000                      | 45000        | 45000
                       :2009   | 6000            | 0                   | -1               | 95000                      | 44000        | 89000
                       :2010   | 7000            | 0                   | -1               | 139000                     | 43000        | 132000
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
                       :2008   | 5000            | 0                   | -1               | 50000                      | 45000        | 45000
                       :2009   | 6000            | 0                   | -1               | 95000                      | 44000        | 89000
                       :2010   | 7000            | 0                   | -1               | 139000                     | 43000        | 132000
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
                       :2008   | 5000            | 0                   | -1               | 50000                      | 45000        | 45000
                       :2009   | 6000            | 0                   | -1               | 95000                      | 44000        | 89000
                       :2010   | 7000            | 0                   | -1               | 139000                     | 43000        | 132000
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
                       :2008   | 0               | 0                   | -1               | 50000                      | 50000        | 50000
                       :2009   | 0               | 0                   | -1               | 100000                     | 50000        | 100000
                       :2010   | 0               | 0                   | -1               | 150000                     | 50000        | 150000
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
                       :2008   | 5000            | 0                   | -1               | 50000                      | 45000        | 45000
                       :2009   | 6000            | 0                   | -1               | 95000                      | 44000        | 89000
                       :2010   | 7000            | 0                   | -1               | 139000                     | 43000        | 132000
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
                       :2008   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2009   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2010   | 50000           | 0                   | -1               | 50000                      | 0            | 0
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
                       :2008   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2009   | 50000           | 0                   | -1               | 50000                      | 0            | 0
                       :2010   | 50000           | 0                   | -1               | 50000                      | 0            | 0
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
                       :2008   | 51000           | 1000                | -1               | 50000                      | 0            | 0
                       :2009   | 51000           | 1000                | -1               | 50000                      | 0            | 0
                       :2010   | 51000           | 1000                | -1               | 50000                      | 0            | 0
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
        // LN 38000 in period 2 CCF should be 37900
        val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                       :2008   | 90000           | 40000               | -1               | 50000                      | 0            | 0
                       :2009   | 30000           | 0                   | -1               | 50000                      | 20000        | 20000
                       :2010   | 21000           | 0                   | -1               | 70000                      | 29000        | 49000
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

  "Scenario 14 Group 1 P1 0 P2 75k" should {
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
      // P2 CCF 10k should be 0k as no cf
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

  "Scenario 15 Group 1 P1 35k P2 0" should {
    "when defined benefit Period 1 is 35k return expected results" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 45000           | 0                   | 0                | 200000                     | 5000         | 105000
                     :2013   | 40000           | 0                   | 0                | 155000                     | 10000        | 65000
                     :2014   | 25000           | 0                   | 0                | 105000                     | 15000        | 30000
                     :2015P1 | 35000           | 0                   | 0                | 110000                     | 40000        | 70000
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 35k and Period 2 is 0 return expected results" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 45000           | 0                   | 0                | 200000                     | 5000         | 105000
                     :2013   | 40000           | 0                   | 0                | 155000                     | 10000        | 65000
                     :2014   | 25000           | 0                   | 0                | 105000                     | 15000        | 30000
                     :2015P1 | 35000           | 0                   | 0                | 110000                     | 40000        | 70000
                     :2015P2 | 0               | 0                   | 0                | 70000                      | 40000        | 65000
                     :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 16 Group 1 Period 1 is 45k and Period 2 is 20k" should {
    "when defined benefit Period 1 is 45k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 45000           | 0                   | 0                | 80000                      | 35000        | 35000
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 45k and Period 2 is 20k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 45000           | 0                   | 0                | 80000                      | 35000        | 35000
                     :2015P2 | 20000           | 0                   | 0                | 35000                      | 15000        | 15000
                     :""".stripMargin(':')
    }
  }

  "Scenario 17 Group 1 Period 1 is 90k and Period 2 is 0k" should {
    "when defined benefit Period 1 is 90k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 90000           | 10000               | 10000            | 80000                      | 0            | 0
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 90000           | 10000               | 10000            | 80000                      | 0            | 0
                     :2015P2 | 0               | 0                   | 0                | 0                          | 0            | 0
                     :""".stripMargin(':')
    }
  }

  "Scenario 18 Group 1 Period 1 is 90k and Period 2 is 25k" should {
    "when defined benefit Period 1 is 90k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 90000           | 10000               | 10000            | 80000                      | 0            | 0
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     :2015P1 | 90000           | 10000               | 10000            | 80000                      | 0            | 0
                     :2015P2 | 25000           | 25000               | 25000            | 0                          | 0            | 0
                     :""".stripMargin(':')
    }
  }

  "Scenario 19 Group 1 Period 1 is 90k and Period 2 is 75k" should {
    "when defined benefit Period 1 is 90k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 30000           | 0                   | 0                | 200000                     | 20000        | 120000
                     :2013   | 40000           | 0                   | 0                | 170000                     | 10000        | 80000
                     :2014   | 35000           | 0                   | 0                | 120000                     | 5000         | 35000
                     :2015P1 | 90000           | 10000               | 0                | 115000                     | 0            | 25000
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 30000           | 0                   | 0                | 200000                     | 20000        | 120000
                     :2013   | 40000           | 0                   | 0                | 170000                     | 10000        | 80000
                     :2014   | 35000           | 0                   | 0                | 120000                     | 5000         | 35000
                     :2015P1 | 90000           | 10000               | 0                | 115000                     | 0            | 25000
                     :2015P2 | 75000           | 75000               | 50000            | 25000                      | 0            | 0
                     :""".stripMargin(':')
      // LN to do broken doTest(table)
    }
  }

  "Scenario 20 Group 1 P1 65k P2 20k" should {
    "when defined benefit Period 1 is 65k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                    :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                    :2015P1 | 65000           | 0                   | 0                | 80000                      | 15000        | 15000
                    :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 65k and Period 2 is 20k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     2015P1 | 65000           | 0                   | 0                | 80000                      | 15000        | 15000
                    :2015P2 | 20000           | 5000                | 5000             | 15000                      | 0            | 0
                    :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 21 Group 1 P1 20k P2 10k" should {
    "when defined benefit Period 1 is 65k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                    :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                    :2015P1 | 20000           | 0                   | 0                | 80000                      | 40000        | 40000
                    :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 20k and Period 2 is 10k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     2015P1 | 20000           | 0                   | 0                | 80000                      | 40000        | 40000
                    :2015P2 | 10000           | 0                   | 0                | 40000                      | 30000        | 30000
                    :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 22 Group 1 P1 20k P2 45k" should {
    "when defined benefit Period 1 is 65k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                    :2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                    :2015P1 | 20000           | 0                   | 0                | 80000                      | 40000        | 40000
                    :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 20k and Period 2 is 45k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     2014   | 40000           | 0                   | 0                | 90000                      | 0            | 0
                     2015P1 | 20000           | 0                   | 0                | 80000                      | 40000        | 40000
                    :2015P2 | 45000           | 5000                | 5000             | 40000                      | 0            | 0
                    :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 23 Group 1 Period 1 is 20k and Period 2 is 75k" should {
    "when defined benefit Period 1 is 20k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 30000           | 0                   | 0                | 90000                      | 10000        | 10000
                     :2015P1 | 20000           | 0                   | 0                | 90000                      | 40000        | 50000
                     :""".stripMargin(':')
      doTest(table)
    }

    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                     :2013   | 50000           | 0                   | 0                | 150000                     | 0            | 50000
                     :2014   | 30000           | 0                   | 0                | 90000                      | 10000        | 10000
                     :2015P1 | 20000           | 0                   | 0                | 90000                      | 40000        | 50000
                     :2015P2 | 75000           | 35000               | 25000            | 50000                      | 0            | 0
                     :""".stripMargin(':')
      // LN to do broken doTest(table)
    }
  }

  "Scenario 24 Group 1 Period 2 is 90k" should {
    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                     :2013   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                     :2014   | 0               | 0                   | 0                | 190000                     | 40000        | 140000
                     :2015P1 | 0               | 0                   | 0                | 220000                     | 40000        | 180000
                     :2015P2 | 90000           | 50000               | 0                | 180000                     | 0            | 90000
                     :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 25 Group 1 Period 2 is 90k" should {
    "when defined benefit Period 1 is 90k and Period 2 is 0k return expected result" in {
      val table = """:year   | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                     :2012   | 0               | 0                   | 0                | 200000                     | 50000        | 150000
                     :2013   | 30000           | 0                   | 0                | 200000                     | 20000        | 120000
                     :2014   | 30000           | 0                   | 0                | 160000                     | 10000        | 80000
                     :2015P1 | 0               | 0                   | 0                | 160000                     | 40000        | 120000
                     :2015P2 | 90000           | 50000               | 0                | 120000                     | 0            | 30000
                     :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 26 Group 1 P1 20k P2 10k" should {
    "when customer is not a scheme member during Period 1 return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 30000           | 0                   | 0                | 150000                     | 20000        | 70000
                    :2014   | 30000           | 0                   | 0                | 110000                     | 10000        | 30000
                    :2015P1 | 0               | 0                   | 0                | 110000                     | 40000        | 70000
                    :""".stripMargin(':')
      doTest(table)
    }

    "when customer is not a scheme member during Period 1 and Period 2 is 20k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 30000           | 0                   | 0                | 150000                     | 20000        | 70000
                    :2014   | 30000           | 0                   | 0                | 110000                     | 10000        | 30000
                    :2015P1 | 0               | 0                   | 0                | 110000                     | 40000        | 70000
                    :2015P2 | 20000           | 0                   | 0                | 70000                      | 20000        | 50000
                    :""".stripMargin(':')
      doTest(table)
    }
  }

  "Scenario 27 Group 1 P1 100k P2 20k" should {
    "when defined benefit Period 1 is 100k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 40000           | 0                   | 0                | 150000                     | 10000        | 60000
                    :2014   | 35000           | 0                   | 0                | 100000                     | 5000         | 15000
                    :2015P1 | 100000          | 20000               | 5000             | 95000                      | 0            | 0
                    :""".stripMargin(':')
      //doTest(table)
    }

    "when defined benefit Period 1 is 100k and Period 2 is 20k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 50000           | 0                   | 0                | 200000                     | 0            | 100000
                    :2013   | 40000           | 0                   | 0                | 150000                     | 10000        | 60000
                    :2014   | 35000           | 0                   | 0                | 100000                     | 5000         | 15000
                    :2015P1 | 100000          | 20000               | 5000             | 95000                      | 0            | 0
                    :2015P2 | 20000           | 30000               | 25000            | 0                          | 0            | 0
                    :""".stripMargin(':')
      //doTest(table)
    }
  }

  "Scenario 28 Group 1 P1 100k P2 45k" should {
     "when defined benefit Period 1 is 100k return expected results" in {
     val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                   :2012   | 35000           | 0                   | 0                | 200000                     | 15000        | 115000
                   :2013   | 20000           | 0                   | 0                | 165000                     | 30000        | 95000
                   :2014   | 30000           | 0                   | 0                | 135000                     | 10000        | 55000
                   :2015P1 | 100000          | 20000               | 0                | 135000                     | 0            | 35000
                   :""".stripMargin(':')
     //doTest(table)
   }

   "when defined benefit Period 1 is 100k and Period 2 is 45k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                   :2012   | 35000           | 0                   | 0                | 200000                     | 15000        | 115000
                   :2013   | 20000           | 0                   | 0                | 165000                     | 30000        | 95000
                   :2014   | 30000           | 0                   | 0                | 135000                     | 10000        | 55000
                   :2015P1 | 100000          | 20000               | 0                | 135000                     | 0            | 35000
                   :2015P2 | 45000           | 45000               | 10000            | 35000                      | 0            | 0
                   :""".stripMargin(':')
      //doTest(table)
    }
  }

  "Scenario 29 Group 1 P1 95k P2 30k" should {
    "when defined benefit Period 1 is 95k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 30000           | 0                   | 0                | 200000                     | 20000        | 120000
                    :2013   | 15000           | 0                   | 0                | 170000                     | 35000        | 105000
                    :2014   | 30000           | 0                   | 0                | 145000                     | 10000        | 65000
                    :2015P1 | 95000           | 15000               | 0                | 145000                     | 0            | 50000
                    :""".stripMargin(':')
      //doTest(table)
    }

    "when defined benefit Period 1 is 95k and Period 2 is 30k return expected results" in {
      val table = """:year  | Defined Benefit | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward
                    :2012   | 30000           | 0                   | 0                | 200000                     | 20000        | 120000
                    :2013   | 15000           | 0                   | 0                | 170000                     | 35000        | 105000
                    :2014   | 30000           | 0                   | 0                | 145000                     | 10000        | 65000
                    :2015P1 | 95000           | 15000               | 0                | 145000                     | 0            | 50000
                    :2015P2 | 30000           | 30000               | 0                | 50000                      | 0            | 20000
                    :""".stripMargin(':')
      //doTest(table)
    }
  }
}

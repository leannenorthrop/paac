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

import calculators._
import calculators.periods._
import calculators.results._
import models._

import org.scalatest._
import uk.gov.hmrc.play.test.UnitSpec

trait Group2TestBase extends Informing {
  this: Suite =>

  def group2Contributions(table: String): List[Contribution] = {
    val years = table.split('\n').drop(2).toList.map(_.split('|').toList(0).trim)
    val definedBenefit = table.split('\n').drop(2).toList.map(_.split('|').toList(1).trim.toLong)
    val moneyPurchase = table.split('\n').drop(2).toList.map(_.split('|').toList(2).trim.toLong)
    val isTriggered = table.split('\n').drop(2).toList.map(_.split('|').toList(3).trim.toBoolean)
    val inputs = Map(years.zip((definedBenefit,moneyPurchase,isTriggered).zipped.toList): _*)    
    val contributions = Utilities.generateDBandMPContributions(inputs).sortBy(_.taxPeriodStart.year)
    contributions
  }  

  def doGroup2Test(table: String, print: Boolean = false): Unit = {
    val results = PensionAllowanceCalculator.calculateAllowances(group2Contributions(table))
    if (print) info(Utilities.toString(results))
    Utilities.assertResults(table, results, false)
  }
}

class Group2CalculationsSpec extends FunSpec with Group2TestBase {
  val dir = "./test/assets/calculators/group2"

  describe ("Group 2") {
    info(s"Tests in $dir:")
    val tests = Utilities.getListOfFiles(dir)
    tests foreach { case (testFilename) =>
      val maybeFileContents = Utilities.readTextFile(testFilename)
      if (maybeFileContents.isDefined) {
        val lines = maybeFileContents.get
        val filename = testFilename.split(java.io.File.separator).reverse(0)
        it (s"$filename: ${lines(0)}") {
          doGroup2Test(lines.mkString("\n"), false)
        }
      }
    }
  }
}

class Group2CalculationsUnitSpec extends UnitSpec with Group2TestBase {

  "Group 2 calculators" should {
    "in Period 2" can {     
      "do Scenario 14 using trigger date" in {
        val table = """:
                       :year    | Defined Benefit | Money Purchase  | Is Triggered | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward | MPAA 
                       :2012    | 50000           | 0               | false        | 0                   | 0                | 200000                     | 0            | 100000                   | 0
                       :2013    | 50000           | 0               | false        | 0                   | 0                | 150000                     | 0            | 50000                    | 0
                       :2014    | 40000           | 0               | false        | 0                   | 0                | 90000                      | 0            | 0                        | 0
                       :2015P1B | -1              | 15000           | false        | 0                   | 0                | 80000                      | 40000        | 40000                    | 0
                       :2015P1A | -1              | 18000           | true         | 0                   | 0                | 80000                      | 40000        | 40000                    | 2000
                       :2015P2A | -1              | 1000            | true         | 0                   | 0                | 40000                      | 39000        | 39000                    | 0
                       :""".stripMargin(':')
        val contributionP1PreTrigger = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod(2015, 4, 23), Some(InputAmounts(None, Some(1500000L), None, Some(false))))
        val contributionP1PostTrigger = Contribution(PensionPeriod(2015, 4, 24), PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(None, Some(1800000L), None, Some(true))))
        val contributionP2PostTrigger = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(None, Some(100000L), None, Some(true))))
        val contributions = group2Contributions(table).slice(0, 3) ++ List(contributionP1PreTrigger, contributionP1PostTrigger, contributionP2PostTrigger)
        val results = PensionAllowanceCalculator.calculateAllowances(contributions)
        Utilities.assertResults(table, results, false)
      }

      "do Scenario 14 using trigger date in p2" in {
        val table = """:
                       :year    | Defined Benefit | Money Purchase  | Is Triggered | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward | MPAA 
                       :2012    | 50000           | 0               | false        | 0                   | 0                | 200000                     | 0            | 100000                   | 0
                       :2013    | 50000           | 0               | false        | 0                   | 0                | 150000                     | 0            | 50000                    | 0
                       :2014    | 40000           | 0               | false        | 0                   | 0                | 90000                      | 0            | 0                        | 0
                       :2015P1B | -1              | 15000           | false        | 0                   | 0                | 80000                      | 40000        | 40000                    | 0
                       :2015P2B | -1              | 18000           | false        | 0                   | 0                | 40000                      | 22000        | 22000                    | 0
                       :2015P2A | -1              | 1000            | true         | 0                   | 0                | 22000                      | 21000        | 21000                    | 0
                       :""".stripMargin(':')
        val contributionP1PreTrigger = Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(None, Some(1500000L), None, Some(false))))
        val contributionP2PreTrigger = Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod(2015, 10, 24), Some(InputAmounts(None, Some(1800000L), None, Some(false))))
        val contributionP2PostTrigger = Contribution(PensionPeriod(2015, 10, 25), PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(None, Some(100000L), None, Some(true))))
        val contributions = group2Contributions(table).slice(0, 3) ++ List(contributionP1PreTrigger, contributionP2PreTrigger, contributionP2PostTrigger)
        val results = PensionAllowanceCalculator.calculateAllowances(contributions)
        Utilities.assertResults(table, results, false)
      }
    }
  }
}

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
import TestUtilities._

import org.scalatest._

class Group3CalculationsSpec extends FunSpec {
  val dir = "./test/assets/calculators/group3"

  def group2Contributions(table: String): List[Contribution] = {
    val years = table.split('\n').drop(2).toList.map(_.split('|').toList(0).trim)
    val definedBenefit = table.split('\n').drop(2).toList.map(_.split('|').toList(1).trim.toLong)
    val moneyPurchase = table.split('\n').drop(2).toList.map(_.split('|').toList(2).trim.toLong)
    val isTriggered = table.split('\n').drop(2).toList.map(_.split('|').toList(3).trim.toBoolean)
    val inputs = Map(years.zip((definedBenefit,moneyPurchase,isTriggered).zipped.toList): _*)    
    val contributions = generateDBandMPContributions(inputs).sortBy(_.taxPeriodStart.year)
    contributions
  }  

  def doGroup3Test(table: String, print: Boolean = false): Unit = {
    val results = PensionAllowanceCalculator.calculateAllowances(group2Contributions(table))
    if (print) info(TestUtilities.toString(results))
    assertResults(table, results, false)
  }

  describe ("Group 3") {
    info(s"Tests in $dir:")
    val tests = getListOfFiles(dir)
    tests foreach { case (testFilename) =>
      val maybeFileContents = readTextFile(testFilename)
      if (maybeFileContents.isDefined) {
        val lines = maybeFileContents.get
        val filename = testFilename.split(java.io.File.separator).reverse(0)
        it (s"$filename: ${lines(0)}") {
          doGroup3Test(lines.mkString("\n"), false)
        }
      }
    }
  }

  /* Scenario 26 contents to be checked with the business
:year    | Defined Benefit | Money Purchase  | Is Triggered | Amount Exceeding AA | Liable to Charge | Available Annual Allowance | Unused AA CF | Cumulative Carry Forward | MPAA 
:2012    | 10000           | 5000            | false        | 0                   | 0                | 200000                     | 35000        | 135000                   | 0
:2013    | 20000           | 3000            | false        | 0                   | 0                | 185000                     | 27000        | 112000                   | 0
:2014    | 20000           | 6000            | false        | 0                   | 0                | 152000                     | 14000        | 76000                    | 0
:2015P1B | 120000          | 5000            | false        | 45000               | 0                | 156000                     | 0            | 31000                    | 0
:2015P1A | 0               | 27000           | true         | 72000               | 7000             | 156000                     | 0            | 11000                    | 0
:2015P2A | 15000           | 5000            | true         | 0                   | 16000            | 11000                      | 0            | 0                        | 0
*/
}

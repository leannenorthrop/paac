/*
 * Copyright 2017 HM Revenue & Customs
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
import calculators.internal._
import calculators.results._
import models._
import TestUtilities._
import org.scalatest.BeforeAndAfterAll
import org.scalatest._
import play.api.Play
import play.api.test.{FakeApplication}

class CalculationsSpec extends FunSpec with BeforeAndAfterAll {
  val dir = "./test/assets/calculators/tapered"
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop(app)
  }

  def contributions(table: String): List[Contribution] = {
    val tableLines = table.split('\n').drop(2).toList
    val years = tableLines.map(_.split('|').toList(0).trim)
    val definedBenefit = tableLines.map(_.split('|').toList(1).trim.toLong)
    val moneyPurchase = tableLines.map(_.split('|').toList(2).trim.toLong)
    val income = tableLines.map(_.split('|').toList(3)).map((v)=>if(v.trim.isEmpty) None else Some(v.trim.toLong*100))
    val isTriggered = tableLines.map(_.split('|').toList(4).trim.toBoolean)
    val inputs = Map(years.zip((definedBenefit,moneyPurchase).zipped.toList.zip((income,isTriggered).zipped.toList).map((t)=>(t._1._1,t._1._2,t._2._1,t._2._2))): _*)
    val contributions = generate(inputs).sortBy(_.taxPeriodStart.year)
    //info(contributions.mkString("\n"))
    contributions
  }

  def doTest(table: String, print: Boolean = false): Unit = {
    val results = PensionAllowanceCalculator.calculateAllowances(contributions(table),missingRowsAreRegistered=false)
    if (print) info(TestUtilities.toString(results))
    assertResults(table, results, false)
  }

  describe ("Tapered Allowance") {
    info(s"Tests in $dir:")
    val tests = getListOfFiles(dir)
    tests foreach {
      (testFilename) =>
      val maybeFileContents = readTextFile(testFilename)
      if (maybeFileContents.isDefined) {
        val lines = maybeFileContents.get
        val filename = testFilename.split(java.io.File.separator).reverse(0)
        it (s"$filename: ${lines(0)}") {
          doTest(lines.mkString("\n"), true)
        }
      }
    }
  }
}

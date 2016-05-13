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

import play.api.Play
import org.scalatest._
import play.api.test.{FakeApplication}

class Group1CalculationsSpec extends FunSpec with BeforeAndAfterAll{
  val dir = "./test/assets/calculators/group1"
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

  describe ("Group 1") {
    info(s"Tests in $dir:")
    val tests = Utilities.getListOfFiles(dir)
    tests foreach { case (testFilename) =>
      val maybeFileContents = Utilities.readTextFile(testFilename)
      if (maybeFileContents.isDefined) {
        val lines = maybeFileContents.get
        val filename = testFilename.split(java.io.File.separator).reverse(0)
        it (s"$filename: ${lines(0)}") {
          val table = lines.mkString("\n")
          val years = table.split("\n").drop(2).toList.map(_.split('|').toList(0).trim)
          val definedBenefit = table.split("\n").drop(2).toList.map(_.split('|').toList(1).trim.toLong)
          val inputs = Map(years.zip(definedBenefit): _*)
          val results = PensionAllowanceCalculator.calculateAllowances(Utilities.generateContributions(inputs))
          Utilities.assertResults(table, results, false)
          if (false) info(Utilities.toString(results))
        }
      }
    }
  }
}

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

import models._
import org.scalatest.Assertions._
import scala.util._
import calculators.periods._
import io._

object TestUtilities {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
    try {
        f(resource)
    } finally {
        resource.close()
    }
  }

  def getListOfFiles(dir: String):List[String] = {
    val d = new java.io.File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList.map(_.getAbsolutePath())
    } else {
      List[String]()
    }
  }

  def readTextFile(filename: String): Option[List[String]] = {
    try {
        val lines = using(io.Source.fromFile(filename)) { source =>
            (for (line <- source.getLines) yield line).toList
        }
        Some(lines)
    } catch {
        case e: Exception => None
    }
  }

  def writeTextFile(dir: String, name: String, contents: String): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(dir + java.io.File.separator + name))
    pw.write(contents)
    pw.close
  }

  def generateContributions(map:Map[String,Long]): List[Contribution] = {
    map.keys.toList.map {
      (key)=>
      key match {
        case "2015P1" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(map(key)*100L))) 
        case "2015P2" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(map(key)*100L))) 
        case "2015P1B" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(map(key)*100L), None, None, Some(false)))) 
        case "2015P1A" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(map(key)*100L), None, None, Some(true)))) 
        case "2015P2B" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(map(key)*100L), None, None, Some(false)))) 
        case "2015P2A" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(map(key)*100L), None, None, Some(true)))) 
        case _ => Contribution(key.toInt, map(key)*100L)
      }
    }
  }

  def generateDBandMPContributions(map:Map[String,(Long,Long,Boolean)]): List[Contribution] = {
    map.keys.toList.map {
      (key)=>
      val maybeDB = if (map(key)._1 == -1) {
        None
      } else {
        Some(map(key)._1*100L)
      }
      val maybeDC = if (map(key)._2 == -1) {
        None
      } else {
        Some(map(key)._2*100L)
      }
      key match {
        case "2015P1" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, None)))
        case "2015P2" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, None)))
        case "2015P1B" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, Some(false))))
        case "2015P1A" => Contribution(PensionPeriod.PERIOD_1_2015_START, PensionPeriod.PERIOD_1_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, Some(true))))
        case "2015P2B" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, Some(false))))
        case "2015P2A" => Contribution(PensionPeriod.PERIOD_2_2015_START, PensionPeriod.PERIOD_2_2015_END, Some(InputAmounts(maybeDB, maybeDC, None, Some(true))))
        case _ => Contribution(key.toInt, Some(InputAmounts(maybeDB, maybeDC, None, Some(false))))
      }
    }
  }

  def toString(results: Seq[TaxYearResults]): String = {
    val headings = List("Year","DB","MP","Chargable",">AA", "AA", "Unused AA", "AACF", "CCF","DBIST","MPIST","ACA","DCA",">MPAA",">AAA","Unused AAA", "Unused MPAA").map((h)=>f"${h}%10s").mkString(" ")
    var message: String = f"\n${headings}\n"
    results.foreach {
      (result)=>
      val values = if (!result.summaryResult.isInstanceOf[ExtendedSummaryFields]) {
        List(result.input.amounts.get.definedBenefit.getOrElse(0L),
            result.input.amounts.get.moneyPurchase.getOrElse(0L),
            result.summaryResult.chargableAmount,
            result.summaryResult.exceedingAAAmount,
            result.summaryResult.availableAllowance,
            result.summaryResult.unusedAllowance,
            result.summaryResult.availableAAWithCF,
            result.summaryResult.availableAAWithCCF,
            0L,
            0L
            ).map(_ / 100.00).map((v)=>f"${v}%10.2f").mkString(" ")
      } else {
        val v = result.summaryResult.asInstanceOf[ExtendedSummaryFields]
        List(result.input.amounts.get.definedBenefit.getOrElse(0L),
            result.input.amounts.get.moneyPurchase.getOrElse(0L),
            v.chargableAmount,
            v.exceedingAAAmount,
            v.availableAllowance,
            v.unusedAllowance,
            v.availableAAWithCF,
            v.availableAAWithCCF,
            v.dbist,
            v.mpist,
            v.alternativeChargableAmount,
            v.defaultChargableAmount,
            v.exceedingMPAA,
            v.exceedingAAA,
            v.unusedAAA,
            v.unusedMPAA
            ).map(_ / 100.00).map((v)=>f"${v}%10.2f").mkString(" ")
      }
      message += f"${result.input.label}%-10s ${values}\n"
    }
    message += "\n\n"
    message
  }

  def assertResults(table:String, results:Seq[TaxYearResults], print:Boolean = false):Unit = {
    def getInts() = {
      table.split("\n").drop(2).toList.map(_.split('|').toList.map{
        (v)=>
        if (v.contains("2015P1B")) 
          4 
        else if (v.contains("2015P1A")) 
          5 
        else if (v.contains("2015P1")) 
          2015 
        else if (v.contains("2015P2B")) 
          6 
        else if (v.contains("2015P2A")) 
          7 
        else if (v.contains("2015P2")) 
          15 
        else { 
          Try[Int](v.trim.toInt) match {
            case Success(v) => v
            case Failure(_) => -2
          }
        }
      })
    }

    if (print) println(TestUtilities.toString(results))

    val valueFor = Map("Amount Exceeding AA"-> { (r:TaxYearResults) => r.summaryResult.exceedingAAAmount },
                       "Liable to Charge"-> { (r:TaxYearResults) => r.summaryResult.chargableAmount },
                       "Available Annual Allowance"-> { (r:TaxYearResults) => r.summaryResult.availableAAWithCF },
                       "Unused AA CF"-> { (r:TaxYearResults) => r.summaryResult.unusedAllowance },
                       "Cumulative Carry Forward"-> { (r:TaxYearResults) => r.summaryResult.availableAAWithCCF },
                       "Available Allowance"-> { (r:TaxYearResults) => r.summaryResult.availableAllowance },
                       "MPAA"-> { 
                        (r:TaxYearResults) => 
                        if (r.summaryResult.isInstanceOf[ExtendedSummaryFields])
                          r.summaryResult.asInstanceOf[ExtendedSummaryFields].unusedMPAA
                        else 
                          0L
                       }
                       )
    val headings = table.split("\n")(1).split('|').map(_.trim)
    val expectedResults = getInts
    val expected = expectedResults.map(headings.zip(_).groupBy(_._1).map{case (k,v)=>(k,v.map(_._2))})
    expected.foreach {
      (row) =>
      val year = row("year")(0)
      val result = year match {
        case 4 => results.find((r)=>r.input.isPeriod1() && r.input.label.contains("B")).get
        case 5 => results.find((r)=>r.input.isPeriod1() && r.input.label.contains("A")).get
        case 6 => results.find((r)=>r.input.isPeriod2() && r.input.label.contains("B")).get
        case 7 => results.find((r)=>r.input.isPeriod2() && r.input.label.contains("A")).get
        case 15 => results.find(_.input.isPeriod2()).get
        case 2015 => results.find(_.input.isPeriod1()).get
        case _ => results.find(_.input.taxPeriodStart.year == year).get
      }
      row.foreach {
        case (k:String,v:Array[Int])=>
        if (k != "year" && k != "Defined Benefit" && k != "Defined Contribution" && k != "Money Purchase" && k != "Is Triggered")
          assertResult(if (v(0) != (-1)) v(0)*100 else v(0),s"${result.input.label} ${k} (Pound values Expected: ${v(0)} Got: ${valueFor(k)(result)/100})")(valueFor(k).apply(result))
      }
    }
  }
}

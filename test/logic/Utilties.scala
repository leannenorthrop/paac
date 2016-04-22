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

import models._
import org.scalatest.Assertions._

object Utilities {
  def generateContributions(map:Map[String,Long]): List[Contribution] = {
    map.keys.toList.map {
      (key)=>
      key match {
        case "2015P1" => Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(map(key)*100L))) 
        case "2015P2" => Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(map(key)*100L))) 
        case _ => Contribution(key.toInt, map(key)*100L)
      }
    }
  }

  def generateMPContributions(map:Map[String,Long]): List[Contribution] = {
    map.keys.toList.map {
      (key)=>
      key match {
        case "2015P1" => Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(None, Some(map(key)*100L), None)))
        case "2015P2" => Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(None, Some(map(key)*100L), None)))
        case _ => Contribution(key.toInt, Some(InputAmounts(None, Some(map(key)*100L), None)))
      }
    }
  }

  def generateDBandMPContributions(map:Map[String,(Long,Long)]): List[Contribution] = {
    map.keys.toList.map {
      (key)=>
      key match {
        case "2015P1" => Contribution(TaxPeriod.PERIOD_1_2015_START, TaxPeriod.PERIOD_1_2015_END, Some(InputAmounts(Some(map(key)._1*100L), Some(map(key)._2*100L), None)))
        case "2015P2" => Contribution(TaxPeriod.PERIOD_2_2015_START, TaxPeriod.PERIOD_2_2015_END, Some(InputAmounts(Some(map(key)._1*100L), Some(map(key)._2*100L), None)))
        case _ => Contribution(key.toInt, Some(InputAmounts(Some(map(key)._1*100L), Some(map(key)._2*100L), None)))
      }
    }
  }

  def toString(results: Seq[TaxYearResults]): String = {
    var message: String = f"\nYear       Defined Benefit  Money Purchase  Chargable  Exceeding AA  Available Allowance   Unused Allowance       AACF             CCF         MPAA          AAA        DBIST        MPIST          ACA          DCA\n"
    results.foreach {
      (result)=>
      message += f"${result.input.taxYearLabel}%-10s ${result.input.amounts.get.definedBenefit.get/100.00}%15.2f ${result.input.amounts.get.moneyPurchase.get/100.00}%15.2f ${result.summaryResult.chargableAmount/100.00}%10.2f ${result.summaryResult.exceedingAAAmount/100.00}%13.2f ${result.summaryResult.availableAllowance/100.00}%20.2f ${result.summaryResult.unusedAllowance/100.00}%18.2f ${result.summaryResult.availableAAWithCF/100.00}%10.2f ${result.summaryResult.availableAAWithCCF/100.00}%15.2f ${result.summaryResult.moneyPurchaseAA/100.00}%12.2f ${result.summaryResult.alternativeAA/100.00}%12.2f ${result.summaryResult.dbist/100.00}%12.2f ${result.summaryResult.mpist/100.00}%12.2f ${result.summaryResult.alternativeChargableAmount/100.00}%12.2f ${result.summaryResult.defaultChargableAmount/100.00}%12.2f\n"
    }
    message += "\n\n"
    message
  }

  def assertResults(table:String, results:Seq[TaxYearResults], print:Boolean = false):Unit = {
    if (print) println(Utilities.toString(results))

    val valueFor = Map("Amount Exceeding AA"-> { (r:TaxYearResults) => r.summaryResult.exceedingAAAmount },
                       "Liable to Charge"-> { (r:TaxYearResults) => r.summaryResult.chargableAmount },
                       "Available Annual Allowance"-> { (r:TaxYearResults) => r.summaryResult.availableAAWithCF },
                       "Unused AA CF"-> { (r:TaxYearResults) => r.summaryResult.unusedAllowance },
                       "Cumulative Carry Forward"-> { (r:TaxYearResults) => r.summaryResult.availableAAWithCCF }
                       )
    val headings = table.split("\n")(0).split('|').map(_.trim)
    val expectedResults = table.split("\n").drop(1).toList.map(_.split('|').toList.map((v)=>if (v.contains("2015P1")) 2015 else if (v.contains("2015P2")) 15 else v.trim.toInt))
    val expected = expectedResults.map(headings.zip(_).groupBy(_._1).map{case (k,v)=>(k,v.map(_._2))})
    expected.foreach {
      (row) =>
      val year = row("year")(0)
      val result = year match {
        case 15 => results.find(_.input.taxPeriodStart == TaxPeriod.PERIOD_2_2015_START).get
        case 2015 => results.find(_.input.taxPeriodStart == TaxPeriod.PERIOD_1_2015_START).get
        case _ => results.find(_.input.taxPeriodStart.year == year).get
      }
      row.foreach {
        case (k:String,v:Array[Int])=>
        if (k != "year" && k != "Defined Benefit" && k != "Defined Contribution" && k != "Money Purchase")
          assertResult(if (v(0) != (-1)) v(0)*100 else v(0),s"${result.input.taxYearLabel} ${k} (converted to pence)")(valueFor(k).apply(result))
      }
    }
  }
}

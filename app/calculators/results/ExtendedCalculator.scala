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

package calculators.results

import models._
import calculators.periods._ 
import calculators.AllowanceCalculator 

protected trait ExtendedCalculator extends BasicCalculator {
  protected def getCalculator(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): calculators.ExtendedSummaryCalculator
  override def summary(implicit previousPeriods:Seq[TaxYearResults], contribution: Contribution): Option[Summary] = {
    if (isSupported(contribution)) {
      val calculator = getCalculator
      Some(ExtendedSummaryFields(calculator.chargableAmount,
                                 calculator.exceedingAllowance,
                                 calculator.annualAllowance,
                                 calculator.unusedAllowance,
                                 calculator.annualAllowanceCF,
                                 calculator.annualAllowanceCCF,
                                 calculator.unusedAAA,
                                 calculator.unusedMPAA,
                                 calculator.moneyPurchaseAA,
                                 calculator.alternativeAA,
                                 calculator.dbist,
                                 calculator.mpist,
                                 calculator.alternativeChargableAmount,
                                 calculator.defaultChargableAmount,
                                 calculator.cumulativeMP,
                                 calculator.cumulativeDB,
                                 calculator.exceedingMPAA,
                                 calculator.exceedingAAA,
                                 calculator.preFlexiSavings,
                                 calculator.postFlexiSavings,
                                 calculator.isMPAAApplicable,
                                 calculator.acaCF,
                                 calculator.dcaCF))
    } else None
  }
}
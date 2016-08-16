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

package models

/** Extends summary result implementation. Used by calculators for years from 2015 onwards. */
case class ExtendedSummaryFields(chargableAmount: Long = 0,
                                 exceedingAAAmount: Long = 0,
                                 availableAllowance: Long = 0,
                                 unusedAllowance: Long = 0,
                                 availableAAWithCF: Long = 0,
                                 availableAAWithCCF: Long = 0,
                                 unusedAAA: Long = 0,
                                 unusedMPAA: Long = 0,
                                 moneyPurchaseAA: Long = 0,
                                 alternativeAA: Long = 0,
                                 dbist: Long = 0,
                                 mpist: Long = 0,
                                 alternativeChargableAmount: Long = 0,
                                 defaultChargableAmount: Long = 0,
                                 cumulativeMP: Long = 0,
                                 cumulativeDB: Long = 0,
                                 exceedingMPAA: Long = 0,
                                 exceedingAAA: Long = 0,
                                 preFlexiSavings: Long = 0,
                                 postFlexiSavings: Long = 0,
                                 isMPA: Boolean = false,
                                 acaCF: Long = 0,
                                 dcaCF: Long = 0,
                                 isACA: Boolean = false,
                                 availableAAAWithCF: Long = 0) extends Summary

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

package models

trait Semigroup[A] {
  def append(a1: A, a2: A): A
}

trait Monoid[A] {
  def zero: A
}

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(ab: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A,B](fa: F[A])(fab: F[A => B]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def lift[A](a: A): F[A]
}

trait Bind[F[_]] extends Apply[F] {
  def bind[A,B](fa: F[A])(afb: A => F[B]): F[B]
}

trait Monad[F[_]] extends Applicative[F] with Bind[F]

sealed trait Period;
case object NilPeriod extends Period;
case class SavingsPeriod(year: Int) extends Period;
case class SavingsPeriod1_2015() extends Period;
case class SavingsPeriod2_2015() extends Period;

sealed trait Parameter[A] extends Monoid[A] with Semigroup[A]

trait MoneyParameter[F[_]] extends Parameter[Amount]
                           with Semigroup[Amount]
                           with Monoid[Amount]
                           with Monad[F] {
  def amount: Amount
  def zero: Amount = 0
  def append(a1: Amount, a2: Amount): Amount = a1 + a2
  def +(a1: MoneyParameter[Parameter], a2: MoneyParameter[Parameter]): MoneyParameter[Parameter] =
    this.lift(a1.amount + a2.amount)
}

sealed trait PreFlexiAccess
sealed trait PostFlexiAccess
case class DefinedBenefit(amount: Amount) extends MoneyParameter[Parameter] with PreFlexiAccess {
  //def lift(amount: Amount): Parameter[Amount] = DefinedBenefit(amount)
}
case class MoneyPurchase(amount: Amount) extends MoneyParameter[Parameter] with PreFlexiAccess
case class TriggeredMoneyPurchase(amount: Amount) extends MoneyParameter[Parameter] with PostFlexiAccess
case class AdjustedIncome(amount: Amount) extends MoneyParameter[Parameter]
case class SavingsRow(period: Period, parameters: Seq[Parameter[Amount]]) {
  private val findPreFlexi: Parameter[Amount] => Boolean = p => p match {
    case n: PreFlexiAccess => true
    case _ => false
  }
  private val findPostFlexi: Parameter[Amount] => Boolean = p => p match {
    case n: PostFlexiAccess => true
    case _ => false
  }
  private val nilAmount = DefinedBenefit(0)
  def preFlexiAccessSavings(): Parameter[Amount] = parameters.filter(findPreFlexi).reduce(_.amount + _.amount)
  def postFlexiAccessSavings(): Parameter[Amount] = parameters.filterNot(findPostFlexi)(0)
}

sealed trait Calc {
  def apply(row: SavingsRow): Calculation
}

object NonChargableCalculation extends Calc {
  def apply(row: SavingsRow): Calculation = (amount, allowances) => 0
}

object ChargableCalculation {
  def apply(row: SavingsRow): Calculation = row match {
    case SavingsRow(SavingsPeriod(year),_) if year < 2011 => NonChargableCalculation(row)

  }
}

package test

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account.{Account, AccountingBalanceEvent, AccountingEvent, Balance, depositEvent, marketGrowthEvent, marketLossEvent, spendEvent}

import scala.reflect.runtime.universe.TypeTag
object Fields {
  implicit class NetBalance(events: Seq[AccountingBalanceEvent]) {
    def net: Double =
      events.foldLeft(0d)((sum, e) =>
        sum + (e match {
          case spendEvent(amt, _, _)        => -amt
          case depositEvent(amt, _, _)      => amt
          case marketGrowthEvent(amt, _, _) => amt
          case marketLossEvent(amt, _, _)   => amt
          case _                            => 0d
        })
      )
  }
  case class Bal[E,A <: dataset[A] with EventLog[E, A]](value: Option[Double])(implicit taga:TypeTag[A],tagself:TypeTag[Bal[E,A]])
      extends (A ==> Bal[E,A])
      with produces[Option[Double]] {
    override def apply(src: dataset[A]): dataset[Bal[E,A]] =
      for {
        model <- src.<--[A]
      } yield {
        val res =
          Bal[E,A](Some(model.value.collect({ case b: AccountingBalanceEvent => b }).net))
        res
      }
  }

  implicit class BalanceGrammer[A <: dataset[A] with EventLog[AccountingEvent,A]](src: dataset[A])(implicit taga: TypeTag[A]) {
    def bal:dataset[Bal[AccountingEvent,A]] = {
      val ctx = data[A]() ++ src
      ctx.<-+[Bal[AccountingEvent,A]]
    }
//    def bal: dataset[Bal[AccountingEvent,Account]] =
//      for {
//        acct <- src.account
//      } yield {
//        val ctx = data[Account]() ++ acct ++ (new Bal[AccountingEvent,Account](None))
//        for {
//          res <- ctx.<-+[Bal[AccountingEvent,Account]]
//        } yield res
//      }
  }
}

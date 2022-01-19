package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import test.Containers.EventBasedBasicContainer
import test.Containers.EventLog
import test.Date._
import test.Event.Event
import test.Fields._

import scala.reflect.runtime.universe.TypeTag
package object Account {

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
  implicit class NetCostBasis(events: Seq[AccountingCostBasisEvent]) {
    def net: Double =
      events.foldLeft(0d)((sum, e) =>
        sum + (e match {
          case increaseCostBasisEvent(amount, _, _) => amount
          case decreaseCostBasisEvent(amount, _, _) => -amount
          case _                                    => 0d
        })
      )
  }

  trait AccountingEvent extends ::[AccountingEvent] with Identifiable with Event {
    val amount: Double
    val accountid: Long
    val id = accountid
    val date: LocalDate
  }
  trait AccountingBalanceEvent extends AccountingEvent

  trait AccountingCostBasisEvent extends AccountingEvent

  trait Account extends ::[Account] with EventLog[Account]
//define your fields and field operations
  trait AccountBalance extends Account {
    //could be replaced by a def but this way we can save some compute on multiple calls within the same context
    lazy val balance: Double = (for {
      bal <- this.BALANCE
    } yield bal).get.value.getOrElse(0d) + initialBalance
    val initialBalance: Double
    def spend(amt: Double, date: Date) =
      if (amt >= balance) throw new Error("balance of account exceeded")
      else this.addEvent(spendEvent(amt, this.id, date))
    def deposit(amt: Double, date: Date) =
      if (amt < 0) throw new Error("cannot deposit a negative amount")
      else this.addEvent(depositEvent(amt, this.id, date))
  }

  trait AccountCostBasis extends Account {
    lazy val costBasis: Double = (for {
      bal <- this.COST_BASIS
    } yield bal).get.value.getOrElse(0d) + initialCostBasis
    val initialCostBasis: Double
  }
  case class spendEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingBalanceEvent
  case class depositEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingBalanceEvent

  case class marketGrowthEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingBalanceEvent
  case class marketLossEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingBalanceEvent

  implicit class AccountAPI[A <: Account](src: dataset[A])(implicit taga: TypeTag[A]) {
    def account: dataset[Account] = if (src.isInstanceOf[Account]) src else src.<--[Account]
  }

  case class increaseCostBasisEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingCostBasisEvent
  implicit def balanceFieldconverter(a: Account): AccountBalance = a match {
    case b: AccountBalance => b
    case _                 => throw new Error(s"Balance not applicable for ${a.toString}")
  }

  case class decreaseCostBasisEvent(amount: Double, accountid: Long, date: LocalDate)
      extends AccountingCostBasisEvent

  implicit def costBasisField(a: Account): AccountCostBasis = a match {
    case c: AccountCostBasis => c
    case _                   => throw new Error(s"Cost Basis not applicable for ${a.toString}")
  }

  case class CheckingAccount(id: Long, initialBalance: Double, value: Seq[Event] = Seq())
      extends Account
      with AccountBalance {
    def addEvent(events: (Event with Identifiable)*): dataset[Account] =
      this.copy(value = events ++ value)
  }

  case class BokerageAccount(
    id: Long,
    initialBalance: Double,
    initialCostBasis: Double = 0d,
    value: Seq[Event] = Seq()
  ) extends Account
      with AccountCostBasis
      with AccountBalance {
    def addEvent(events: (Event with Identifiable)*): dataset[Account] =
      this.copy(value = events ++ value)
  }

  case class IRA(id: Long, initialBalance: Double, value: Seq[Event] = Seq()) extends Account {
    def addEvent(events: (Event with Identifiable)*): dataset[Account] =
      this.copy(value = events ++ value)
  }

  //define collection container for collections of accounts
  case class Accounts(val value: Seq[Account])(
    implicit val taga: TypeTag[Account],
    val tagself: TypeTag[Accounts]
  ) extends EventBasedBasicContainer[Account, Accounts] {
    override def apply(coll: Map[Long, Account]): dataset[Accounts] = Accounts(coll.values.toSeq)
  }

  implicit class AccountsAPI[A <: Accounts](src: dataset[A])(implicit taga: TypeTag[A]) {

    def underlyingAccounts: produces[Seq[Account]] =
      someval((for {
        accounts <- src.accounts
      } yield accounts).getValue.value.toSeq)

    def addEvents(events: AccountingEvent*): dataset[Accounts] =
      for {
        accounts <- src.accounts
      } yield accounts.addEvent(events: _*)

    def getAccount(id: Long): Option[Account] =
      for {
        accounts <- src.accounts.toOption
      } yield accounts.get(id)

    def accounts: dataset[Accounts] = if (src.isInstanceOf[A]) src else src.<--[Accounts]

  }
  implicit class LederOperations[A <: Accounts with Date](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {
    def withdraw(account: AccountBalance, amt: Double): dataset[A] =
      for {
        accounts <- src.accounts
        date <- src.currentDate
        updtdAcct <- accounts.get(account.id).spend(amt, date)
        res <- accounts.update(updtdAcct)
      } yield src ++ res
    def deposit(account: AccountBalance, amt: Double): dataset[A] =
      for {
        accounts <- src.accounts
        date <- src.currentDate
        updtdAcct <- accounts.get(account.id).deposit(amt, date)
        res <- accounts.update(updtdAcct)
      } yield src ++ res
    def recordMarketGrowth(account: BokerageAccount, amt: Double): dataset[A] =
      for {
        accounts <- src.accounts
        date <- src.currentDate
        res <- accounts.addEvents(marketGrowthEvent(amt, account.id, date))
      } yield src ++ res
    def recordMarketLoss(account: BokerageAccount, amt: Double): dataset[A] =
      for {
        accounts <- src.accounts
        date <- src.currentDate
        res <- accounts
          .addEvents(marketLossEvent(amt, account.id, date))
      } yield src ++ res

  }

}

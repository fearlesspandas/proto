package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Date._

import scala.reflect.runtime.universe.TypeTag
object Account {

  trait AccountingEvent extends ::[AccountingEvent]{
    val amount:Double
    val accountid:Long
    val date:LocalDate
  }
  case class spendEvent(amount: Double, accountid: Long,date:LocalDate) extends AccountingEvent
  case class depositEvent(amount: Double, accountid: Long,date:LocalDate) extends AccountingEvent
  case class marketGrowthEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingEvent
  case class marketLossEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingEvent

  trait Account extends ::[Account] with produces[Seq[AccountingEvent]]{
    self =>
    val balance: Double
    val id:Long
    def apply(balance: Double): Account
    def spend(amt:Double) = if(amt > balance) throw new Error("balance of account exceeded") else apply(balance - amt)
    def deposit(amt:Double) = apply(balance + amt)
    def addEvents(events:AccountingEvent*):Account
  }



  case class CheckingAccount(id:Long,balance: Double,value:Seq[AccountingEvent] = Seq()) extends Account {
    override def apply(newbalance: Double): CheckingAccount = this.copy(balance = newbalance)
    override def addEvents(events: AccountingEvent*): Account = this.copy(value = events ++ this.value)
  }

  case class BokerageAccount(id:Long,balance: Double,value:Seq[AccountingEvent] = Seq()) extends Account {
    override def apply(newbalance: Double): BokerageAccount = this.copy(balance = newbalance)
    override def addEvents(events: AccountingEvent*): Account = this.copy(value = events ++ this.value)
  }

  case class IRA(id:Long,balance: Double,value:Seq[AccountingEvent] = Seq()) extends Account {
    override def apply(balance: Double): IRA = this.copy(balance = balance)
    override def addEvents(events: AccountingEvent*): Account = this.copy(value = events ++ this.value)
  }




  //this class illustrates how you would define an operation generally over
  //a dataset or anything contained within it.


  case class Accounts(val value: Seq[Account],eventLog:Seq[AccountingEvent]) extends (Accounts ==> Accounts) with produces[Seq[Account]] {
    override def apply(src: dataset[Accounts]): dataset[Accounts] = src.<--[Accounts]
    private def apply(account:Account):dataset[Accounts] = {
      val acctMap = this.accountMap
      val exists = acctMap.get(account.id).isDefined
      lazy val updatedMap = acctMap.updated(account.id,account)
      val newAcctColl = if(exists) updatedMap.values.toSeq else value :+ account
      new Accounts(newAcctColl,this.eventLog){
        lazy val accountMap = if (acctMap != null) updatedMap else Map(account.id -> account)
      }
    }
    private[Account] def addEvent(events:AccountingEvent*):dataset[Accounts] = {
      events.foldLeft(this)((accumaccts,e) => {
        val acct = accumaccts.get(e.accountid)
        accumaccts.update(acct.addEvents(e)).get
      })
    }
    private lazy val accountMap:Map[Long,Account] = value.map(a => a.id -> a).toMap

    def get(id:Long):Account = accountMap(id)
    def update(acct:Account):dataset[Accounts] = apply(acct)

  }

  implicit class AccountsAPI[A<:Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def accounts:dataset[Accounts] = if(src.isInstanceOf[A]) src else src.<--[Accounts]
    def events:produces[Seq[AccountingEvent]] = src.accounts.biMap[produces[Seq[AccountingEvent]]](err => noVal(err.value:_*))(d => someval(d.get.eventLog ++ d.get.value.flatMap(_.value)))
    def underlyingAccounts:produces[Seq[Account]] = (for{
      accounts <- src.accounts
    }yield accounts).getValue
    private[Account] def addEvents(events:AccountingEvent*):dataset[Accounts] = for{
      accounts <- src.accounts
    }yield accounts.addEvent(events:_*)
    def getAccount(id:Long):Option[Account] = for{
      accounts <- src.accounts.toOption
    }yield accounts.get(id)

  }

  implicit class GenerateSpendByAccount[A<:Account](src:A){
    def generateSpendEvent(amt:Double,id:Long,date:LocalDate) :AccountingEvent= src match{
      case _:CheckingAccount => spendEvent(amt,id,date)
      case _:BokerageAccount => marketLossEvent(amt,id,date)
    }
    def generateDepositEvent(amt:Double,id:Long,date:LocalDate) :AccountingEvent= src match{
      case _:CheckingAccount => depositEvent(amt,id,date)
      case _:BokerageAccount => marketGrowthEvent(amt,id,date)
    }
  }
  implicit class LederOperations[A<:Accounts with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def withdraw(account:Account, amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).spend(amt)).addEvents(spendEvent(amt,account.id,date))
    }yield src ++ res
    def deposit(account:Account,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).deposit(amt)).addEvents(depositEvent(amt,account.id,date))
    }yield src ++ res
    def recordMarketGrowth(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).deposit(amt)).addEvents(marketGrowthEvent(amt,account.id,date))
    }yield src ++ res
    def recordMarketLoss(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts
        .update(accounts.get(account.id).spend(amt))
        .addEvents(marketLossEvent(amt,account.id,date))
    }yield src ++ res

  }


}
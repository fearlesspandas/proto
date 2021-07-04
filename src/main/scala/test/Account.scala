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
  trait Account extends ::[Account] {
    self =>
    val balance: Double
    val id:Long
    def apply(balance: Double): Account
    def spend(amt:Double) = apply(balance - amt)
    def deposit(amt:Double) = apply(balance + amt)
  }



  case class CheckingAccount(id:Long,balance: Double) extends Account {
    override def apply(newbalance: Double): CheckingAccount = CheckingAccount(id,newbalance)
  }

  case class BokerageAccount(id:Long,balance: Double) extends Account {
    override def apply(balance: Double): BokerageAccount = BokerageAccount(id,balance)
  }

  case class IRA(id:Long,balance: Double) extends Account {
    override def apply(balance: Double): IRA = IRA(id,balance)
  }




  //this class illustrates how you would define an operation generally over
  //a dataset or anything contained within it.


  case class Accounts(val value: Seq[Account],eventLog:Seq[AccountingEvent]) extends (Accounts ==> Accounts) with produces[Seq[Account]] {
    override def apply(src: dataset[Accounts]): dataset[Accounts] = src.fetch[Accounts]
    private def apply(account:Account):dataset[Accounts] = {
      val acctMap = this.accountMap
      val exists = acctMap.get(account.id).isDefined
      lazy val updatedMap = acctMap.updated(account.id,account)
      val newAcctColl = if(exists) updatedMap.values.toSeq else value :+ account
      new Accounts(newAcctColl,this.eventLog){
        lazy val accountMap = if (acctMap != null) updatedMap else Map(account.id -> account)
      }
    }
    private[Account] def addEvent(events:AccountingEvent*):dataset[Accounts] = new Accounts(this.value,events ++ this.eventLog)
    private lazy val accountMap:Map[Long,Account] = value.map(a => a.id -> a).toMap

    def get(id:Long):Account = accountMap(id)
    def update(acct:Account):dataset[Accounts] = apply(acct)

  }

  implicit class AccountsAPI[A<:Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def accounts:dataset[Accounts] = if(src.isInstanceOf[A]) src else src.fetch[Accounts]
    def events:Val[Seq[AccountingEvent]] = for{
      accounts <- src.accounts
    }yield Val(accounts.eventLog)//Val(accounts.eventLog)
    def underlyingAccounts:Val[Seq[Account]] = for{
      accounts <- src.accounts
    }yield Val[Seq[Account]](accounts.value)
    private[Account] def addEvents(events:AccountingEvent*):dataset[Accounts] = for{
      accounts <- src.accounts
    }yield accounts.addEvent(events:_*)
    def getAccount(id:Long):Option[Account] = for{
      accounts <- src.accounts.toOption
    }yield accounts.get(id)
    def getAccountModel(id:Long):Val[Account] = (for{
      accounts <- src.accounts
    }yield Val(value = accounts.get(id)))

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
    }yield src.include(res)
    def deposit(account:Account,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).deposit(amt)).addEvents(depositEvent(amt,account.id,date))
    }yield src.include(res)
    def recordMarketGrowth(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).spend(amt)).addEvents(marketGrowthEvent(amt,account.id,date))
    }yield src.include(res)
    def recordMarketLoss(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts
        .update(accounts.get(account.id).spend(amt))
        .addEvents(marketLossEvent(amt,account.id,date))
    }yield src.include(res)

  }


}
package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Date._

import scala.reflect.runtime.universe.TypeTag
package object Account {

  implicit class NetBalance(events:Seq[AccountingBalanceEvent]){
    def net:Double = events.foldLeft(0d)((sum,e) => sum + (e match {
      case spendEvent(amt,_,_) => -amt
      case depositEvent(amt,_,_) => amt
      case marketGrowthEvent(amt,_,_) => amt
      case marketLossEvent(amt,_,_) => amt
      case _ => 0d
    }))
  }
  implicit class NetCostBasis(events:Seq[AccountingCostBasisEvent]){
    def net:Double = events.foldLeft(0d)((sum,e) => sum + (e match {
      case increaseCostBasisEvent(amount,_,_) => amount
      case decreaseCostBasisEvent(amount,_,_) => -amount
      case _ => 0d
    }))
  }

  trait AccountingEvent extends ::[AccountingEvent]{
    val amount:Double
    val accountid:Long
    val date:LocalDate
  }
  trait AccountingBalanceEvent extends AccountingEvent

  case class spendEvent(amount: Double, accountid: Long,date:LocalDate) extends AccountingBalanceEvent
  case class depositEvent(amount: Double, accountid: Long,date:LocalDate) extends AccountingBalanceEvent
  case class marketGrowthEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingBalanceEvent
  case class marketLossEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingBalanceEvent

  trait AccountingCostBasisEvent extends AccountingEvent

  case class increaseCostBasisEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingCostBasisEvent
  case class decreaseCostBasisEvent(amount:Double,accountid:Long,date:LocalDate) extends AccountingCostBasisEvent

  trait Account extends ::[Account] with produces[Seq[AccountingEvent]]{
    self =>
    val id:Long
    def addEvents(events:AccountingEvent*):Account
  }
  implicit class AccountAPI[A<:Account](src:dataset[A])(implicit taga:TypeTag[A]){
    def account:dataset[Account] = if(src.isInstanceOf[Account]) src else src.<--[Account]
    def balanceField:dataset[BalanceField] = for{
      bf <- (src +- BalanceField(0d)).<-+[BalanceField]
    }yield bf
  }
  implicit class datedFields[A<:Account with Date](src:dataset[A])(implicit taga:TypeTag[A]){

  }
  case class BalanceField(value:Double) extends (Account ==> BalanceField) with produces[Double]{
    override def apply(src: dataset[Account]): dataset[BalanceField] = for{
      acct <- src.account
    } yield{
      BalanceField(acct.value.collect({case b:AccountingBalanceEvent => b}).net)
    }
  }
  //define your fields and field operations
  trait Balance extends Account{
    lazy val balance:Double = this.balanceField.get
    def spend(amt:Double,date:Date) = if(amt > balance) throw new Error("balance of account exceeded") else this.addEvents(spendEvent(amt,this.id,date))
    def deposit(amt:Double,date:Date) = this.addEvents(depositEvent(amt,this.id,date))
  }
  implicit def balanceField(a:Account):Balance= a match {
    case b:Balance => b
    case _ => throw new Error(s"Balance not applicable for ${a.toString}")
  }

  trait CostBasis extends Account{
    val costBasis:Double
  }
  implicit def costBasisField(a:Account):CostBasis = a match {
    case c:CostBasis => c
    case _ => throw new Error(s"Cost Basis not applicable for ${a.toString}")
  }

  case class CheckingAccount(id:Long,balance: Double,value:Seq[AccountingEvent] = Seq()) extends Account with Balance{
    override def addEvents(events: AccountingEvent*): Account = {
      val amt = events.collect({case e:AccountingBalanceEvent => e}).net
      this.copy(value = events ++ this.value,balance = this.balance + amt)
    }
  }

  case class BokerageAccount(id:Long,balance: Double,costBasis:Double = 0d,value:Seq[AccountingEvent] = Seq()) extends Account with CostBasis with Balance{
    override def addEvents(events: AccountingEvent*): Account = {
      val amt = events.collect({case e:AccountingBalanceEvent => e}).net
      val costBasisDiff = events.collect({case c:AccountingCostBasisEvent => c}).net
      this.copy(value = events ++ this.value,balance = this.balance + amt,costBasis = this.costBasis + costBasisDiff)
    }
  }

  case class IRA(id:Long,balance: Double,value:Seq[AccountingEvent] = Seq()) extends Account {
    override def addEvents(events: AccountingEvent*): Account = {
      val amt = events.collect({case e:AccountingBalanceEvent => e}).net
      this.copy(value = events ++ this.value,balance = this.balance + amt)
    }
  }




  //this class illustrates how you would define an operation generally over
  //a dataset or anything contained within it.


  case class Accounts(val value: Seq[Account]) extends (Accounts ==> Accounts) with produces[Seq[Account]] {
    override def apply(src: dataset[Accounts]): dataset[Accounts] = src.<--[Accounts]
    private def apply(account:Account):dataset[Accounts] = {
      val acctMap = this.accountMap
      val exists = acctMap.get(account.id).isDefined
      lazy val updatedMap = acctMap.updated(account.id,account)
      val newAcctColl = if(exists) updatedMap.values.toSeq else value :+ account
      new Accounts(newAcctColl){
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
    def events:produces[Seq[AccountingEvent]] = src.accounts.biMap[produces[Seq[AccountingEvent]]](err => noVal(err.value:_*))(d => someval(d.get.value.flatMap(_.value)))
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
  implicit class LederOperations[A<:Accounts with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def withdraw(account:Balance, amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).spend(amt,date))
    }yield src ++ res
    def deposit(account:Balance,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.update(accounts.get(account.id).deposit(amt,date))
    }yield src ++ res
    def recordMarketGrowth(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts.addEvents(marketGrowthEvent(amt,account.id,date))
    }yield src ++ res
    def recordMarketLoss(account:BokerageAccount,amt:Double):dataset[A] = for{
      accounts <- src.accounts
      date <- src.currentDate
      res <- accounts
        .addEvents(marketLossEvent(amt,account.id,date))
    }yield src ++ res

  }


}
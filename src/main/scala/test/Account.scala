package test

import Typical.core.dataset._
import Typical.core.grammar._
import Typical.core.{dataset, grammar}
import src.main.scala.test.Consumption.{Consumption, Counter}
import test.Actions.Action
object Account {

  trait Account {
    self =>
    val balance: Double
    val id:Long

    def apply(balance: Double): Account

    def spend(amt:Double) = apply(balance - amt)
    def deposit(amt:Double) = apply(balance + amt)
  }

  case class CheckingAccount(id:Long,balance: Double) extends Account {
    override def apply(balance: Double): CheckingAccount = CheckingAccount(id,balance)
  }

  case class BokerageAccount(id:Long,balance: Double) extends Account {
    override def apply(balance: Double): BokerageAccount = BokerageAccount(id,balance)
  }

  case class IRA(id:Long,balance: Double) extends Account {
    override def apply(balance: Double): IRA = IRA(id,balance)
  }


  case class GrowAccounts() extends model[Accounts with AccountRates, Accounts] {
    override def apply(src: dataset[Accounts with AccountRates]): dataset[Accounts] = for {
      accounts <- src.fetch[Accounts]
      rates <- src.fetch[AccountRates]
    } yield {
     src.include(Accounts(accounts.value.map(acct => acct.deposit(acct.balance * rates(acct)))))
    }
  }

  import scala.reflect.runtime.universe.TypeTag

  implicit class GrowAccountsGrammar[A<:Accounts with AccountRates](src:dataset[A])(implicit taga:TypeTag[A]){
    def growAccounts:dataset[A] = src.run[GrowAccounts]
  }

  //this class illustrates how you would define an operation generally over
  //a dataset or anything contained within it.


  case class Accounts(val value: Seq[Account]) extends model[Accounts, Accounts] with produces[Seq[Account]] {
    override def apply(src: dataset[Accounts]): dataset[Accounts] = src.fetch[Accounts]
    private def apply(acctMap:Map[Long,Account]):dataset[Accounts] = new Accounts(acctMap.values.toSeq) {
       lazy val accountMap = acctMap
    }
    private lazy val accountMap:Map[Long,Account] = value.map(a => a.id -> a).toMap
    def get(id:Long):Account = accountMap(id)
    def update(acct:Account):dataset[Accounts] = apply(accountMap.updated(acct.id,acct))
  }

  implicit class Spender[A<:Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def accounts:dataset[Accounts] = src.fetch[Accounts]
    def getAccount(id:Long):Option[Account] = for{
      accounts <- src.fetch[Accounts].toOption
    }yield accounts.get(id)
    def getAccountModel(id:Long):dataset[Nothing] with produces[Account] = (for{
      accounts <- src.fetch[Accounts]
    }yield Val(value = accounts.get(id)))
      .asInstanceOf[dataset[Nothing] with produces[Account]]
    def spend(account:Account,amt:Double):dataset[A] = for{
      accounts <- src.fetch[Accounts]
      res <- accounts.update(accounts.get(account.id).spend(amt))
    }yield src.include(res)
    def deposit(account:Account,amt:Double):dataset[A] = for{
      accounts <- src.fetch[Accounts]
      res <- accounts.update(accounts.get(account.id).deposit(amt))
    }yield src.include(res)
    def transfer(from:Account,to:Account,amt:Double):dataset[A] = for{
      accounts <- src.fetch[Accounts]
    }yield{src.spend(accounts.get(from.id),amt).deposit(accounts.get(to.id),amt)}
  }
  type SpendSimDep = Accounts with Consumption with Counter
  case class SpendWildly() extends model[SpendSimDep,Accounts] {
    override def apply(src: dataset[Accounts with Consumption with Counter]): dataset[Accounts] =for{
      accounts <- src.fetch[Accounts]
    }yield{
      accounts.foldLeft(src)((src_, account) => for{
        consumption <- src_.fetch[Consumption]
        counter <- src_.fetch[Counter]
      }yield {
        val amt = consumption.value.map(_.amount).sum
        src_.spend(account,amt)
        .include(consumption).include(counter)
      })
    }
  }
  //case class InputAccounts(value:Seq[Account]) extends Accounts{
  //  override def apply(src: dataset[Accounts]): dataset[Accounts] = ???
  //}

}
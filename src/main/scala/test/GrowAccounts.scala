package test

import Typical.core.grammar._
import Typical.core.dataset._
import test.Account.Accounts
import Date._
import scala.reflect.runtime.universe.TypeTag
case object GrowAccounts extends (Accounts with AccountRates with Date ==> Accounts) {
  override def apply(src: dataset[Accounts with AccountRates with Date]): dataset[Accounts] = for {
    accounts <- src.fetch[Accounts]
    rates <- src.fetch[AccountRates]
    date <- src.fetch[Date]
  } yield {
    val res = accounts.value.foldLeft(src)(
      (accountsAccum,acct) =>{
        val amt = acct.balance * rates(acct)/date.periodsInYear
        if (amt > 0)
        accountsAccum.deposit(acct,amt)
        else
          accountsAccum.spend(acct,amt)
      }
    )
    res
  }
  implicit class GrowAccountsGrammar[A<:Accounts with AccountRates with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def growAccounts:dataset[A] = src.include(GrowAccounts).run[GrowAccounts.type]
  }
}


package test

import Typical.core.grammar._
import Typical.core.dataset._
import test.Account.Accounts
import scala.reflect.runtime.universe.TypeTag
case object GrowAccounts extends (Accounts with AccountRates ==> Accounts) {
  override def apply(src: dataset[Accounts with AccountRates]): dataset[Accounts] = for {
    accounts <- src.fetch[Accounts]
    rates <- src.fetch[AccountRates]
  } yield {
    val res = accounts.value.foldLeft(src)(
      (accountsAccum,acct) =>
        accountsAccum.deposit(acct,acct.balance * rates(acct))
    )
    res
  }
  implicit class GrowAccountsGrammar[A<:Accounts with AccountRates](src:dataset[A])(implicit taga:TypeTag[A]){
    def growAccounts:dataset[A] = src.include(GrowAccounts).run[GrowAccounts.type]
  }
}


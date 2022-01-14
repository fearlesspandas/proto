package test

import Typical.core.grammar._
import Typical.core.dataset._
import test.Account.{Accounts, BokerageAccount}
import Date._
import AccountRates._

import scala.reflect.runtime.universe.TypeTag
package object AccountUtils{
  case object GrowAccounts extends (Accounts with AccountRates with Date ==> Accounts) {
    override def apply(src: dataset[Accounts with AccountRates with Date]): dataset[Accounts] = for {
      accounts <- src.accounts
      rates <- src.<--[AccountRates]
      date <- src.currentDate
    } yield {
      val res = accounts.value.foldLeft(src)(
        (accountsAccum,acct) => acct match {
          case b:BokerageAccount =>
            val amt = b.balance * rates(b)/date.periodsInYear
            if (amt == 0)
              accountsAccum
            else if(amt > 0)
              accountsAccum.recordMarketGrowth(b,amt)
            else
              accountsAccum.recordMarketLoss(b,-amt)
          case _ => accountsAccum
        }
      )
      res
    }
  }

  implicit class GrowAccountsGrammar[A<:Accounts with AccountRates with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def growAccounts:dataset[A] =
      (src +- GrowAccounts).-->[GrowAccounts.type]
  }
}



package test

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account.{Accounts}
import test.SpendEvents.SpendEvents
import Consumption._
import Date._
import EventHandler._
object SpendWildly{
  type SpendSimDep = Accounts with Counter with Consumption with Date
   class SpendWildly() extends (SpendSimDep ==> SpendSimDep) {
    override def apply(src: dataset[SpendSimDep]): dataset[SpendSimDep] = for {
      accounts <- src.<--[Accounts]
      consumption <- src.<-+[SpendEvents]
    } yield
      accounts.foldLeft(
      src.+->[Counter].+->[Consumption]
    )(
      (accumSrc, acct) => accumSrc.withdraw(acct, consumption.value.map(_.amount).sum)
    )
  }
  import scala.reflect.runtime.universe.TypeTag
  implicit class SpendWildlyGrammar[A<:SpendSimDep](src:dataset[A])(implicit taga:TypeTag[A]){
    def spendAggressively:dataset[A] = src.++(new SpendWildly()).-->[SpendWildly.SpendWildly]
  }
}

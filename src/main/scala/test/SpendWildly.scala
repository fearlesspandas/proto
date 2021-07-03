package test

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account.{Accounts}
import test.SpendEvents.SpendEvents
import Consumption._
import EventHandler._
object SpendWildly{
  type SpendSimDep = Accounts with Counter with Consumption
   class SpendWildly() extends (SpendSimDep ==> SpendSimDep) {
    override def apply(src: dataset[Accounts with Counter with Consumption]): dataset[SpendSimDep] = for {
      accounts <- src.fetch[Accounts]
      consumption <- src.derive[SpendEvents]
    } yield
      accounts.foldLeft(
      src.iter[Counter].iter[Consumption]
    )(
      (accumSrc, acct) => accumSrc.spend(acct, consumption.value.map(_.amount).sum)
    )
  }
  import scala.reflect.runtime.universe.TypeTag
  implicit class SpendWildlyGrammar[A<:SpendSimDep](src:dataset[A])(implicit taga:TypeTag[A]){
    def spendAggressively:dataset[A] = src.include(new SpendWildly()).run[SpendWildly.SpendWildly]
  }
}

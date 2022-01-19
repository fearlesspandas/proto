package test

import Typical.core.grammar._
import Typical.core.dataset._
import Consumption._
import test.Account.spendEvent
object SpendEvents{
  type dep = Consumption
  case class SpendEvents(value : Seq[spendEvent]) extends (dep ==> SpendEvents){
    override def apply(src: dataset[dep]): dataset[SpendEvents] =for{
      eventLog <- src.<--[Consumption]
    }yield SpendEvents(
      eventLog.value.collect({case se:spendEvent => se})
    )
  }
}

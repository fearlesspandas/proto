package test

import Typical.core.grammar._
import Typical.core.dataset._
import EventHandler._
object SpendEvents{
  type dep = EventStore
  case class SpendEvents(value : Seq[spendEvent]) extends (dep ==> SpendEvents){
    override def apply(src: dataset[dep]): dataset[SpendEvents] =for{
      eventLog <- src.fetch[EventStore]
    }yield SpendEvents(
      eventLog.value.collect({case se:spendEvent => se})
    )
  }
}

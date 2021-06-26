package test

import Typical.core.grammar._
import Typical.core.dataset._
import src.main.scala.test.EventHandler.{EventStore, Events, spendEvent}
object SpendEvents{
  type dep = EventStore
  case class SpendEvents(value : Seq[spendEvent]) extends model[dep,SpendEvents]{
    override def apply(src: dataset[dep]): dataset[SpendEvents] =for{
      eventLog <- src.fetch[EventStore]
    }yield SpendEvents(
      eventLog.value.collect({case se:spendEvent => se})
    )


  }
}

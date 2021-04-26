package test

import Typical.core.grammar._
import Typical.core.dataset._
import src.main.scala.test.EventHandler.{Events, spendEvent}
object SpendEvents{
  type dep = Events
  case class SpendEvents(value : Seq[spendEvent]) extends model[dep,SpendEvents]{
    override def iterate(src: dataset[dep]): dataset[SpendEvents] =for{
      eventLog <- src.fetch[Events]
    }yield SpendEvents(
      eventLog.value.collect({case se:spendEvent => se})
    )


  }
}

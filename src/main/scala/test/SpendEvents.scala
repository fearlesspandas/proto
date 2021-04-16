package test

import Typical.core.typeable._
import Typical.core.grammar._
import src.main.scala.test.EventHandler.{Events, spendEvent}
object SpendEvents{
  type dep = Events
  case class SpendEvents() extends model[dep,SpendEvents] with produces[Int => Seq[spendEvent]] {
    override def iterate(src: dataset[dep]): Option[SpendEvents] =for{
      eventLog <- src.fetch[Events]
      events = eventLog.value
    }yield new SpendEvents{
      override val value: Int => Seq[spendEvent] = (date:Int ) => events.collect({case se:spendEvent if se.Year == date => se})
    }
    //set initial value
    override val value:Int =>  Seq[spendEvent] = _ => Seq()

  }
}

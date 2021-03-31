package src.main.scala.test

import Typical.core.{grammar, typeable}
import Typical.core.typeable.{TerminalType, dataset, model}

package object EventHandler{
  import grammar._
  import typeable._

  type EventDependencies = Events
  type SpendEventDependencies = Events
  trait Event{
    val amount:Double
  }
  case class spendEvent(amount:Double) extends Event
  case class SpendEvents() extends model[SpendEventDependencies,SpendEvents] with TerminalType[Seq[spendEvent]] {
    override def iterate(src: dataset[SpendEventDependencies]): SpendEvents = new SpendEvents{
      override val value: Seq[spendEvent] = src.fetch[Events].get.currentEventStore.collect({case se:spendEvent => se})
    }
    override val value: Seq[spendEvent] = Seq()
  }

  case class Events() extends model[Events,Events] with TerminalType[Seq[Event]] {
    val currentEventStore:Seq[Event] = Seq()
    override def iterate(src: dataset[EventDependencies]): Events = src.fetch[Events].get

    override val value:Seq[Event] = Seq()
    def addEvents(events:Seq[Event]):Events = {
      val currevents = this.value
      new Events {
        override val value: Seq[Event] = currevents ++ events
      }
    }
  }
  //val ev = dat.fetch[Events]
  //val updtd = ev.addevents(someevents)
  //dat.include[Events](updtd)
  //
}

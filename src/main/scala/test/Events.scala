package src.main.scala.test

import Typical.core.grammar
import Typical.core.dataset._
import src.main.scala.test.Consumption.{Consumption, Counter}
package object EventHandler {
  import grammar._

  trait Event {
    val amount: Double
  }

  case class spendEvent(amount: Double, Year: Int) extends Event

  type dep = EventStore  with Counter

  trait EventStore extends model[EventStore with Counter,Events]{
    val value:Seq[Event]
    val formula:String
  }
   case class Events(
                     val value:Seq[Event],
                     val formula:String
                   ) extends EventStore {
    override def apply(src: dataset[dep]): dataset[Events] =
      for {
        consumptionModel <- src.derive[Consumption]
        currEvents <- src.fetch[EventStore]
      } yield new Events(
          consumptionModel.value ++ currEvents.value,
          currEvents.formula + consumptionModel.value
            .map(e => s" + ${e.amount}")
            .foldLeft("")(_ + _)
        )
  }
}

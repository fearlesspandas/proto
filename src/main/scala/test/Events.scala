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

  type dep = Events with Consumption with Counter

  case class Events(
                     value:Seq[Event],
                    formula:String
                   ) extends Id[Events] with model[
                              Events with Consumption with Counter,
                              Events
                              ]{
    override def iterate(src: dataset[dep]): dataset[Events] =
      for {
        consumptionModel <- src.derive[Consumption]
        currEvents <- src.fetch[Events]
      } yield Events(
          consumptionModel.value ++ currEvents.value,
          currEvents.formula + consumptionModel.value
            .map(e => s" + ${e.amount}")
            .foldLeft("")(_ + _)
        )
  }
}

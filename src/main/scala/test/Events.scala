package src.main.scala.test

import Grammar.Calc
import Typical.core.grammar
import Typical.core.typeable
import src.main.scala.test.Consumption.{Consumption, Counter}
import src.main.scala.test.EventGenerator.EventGenerator
import src.main.scala.test.runner.Sim

import scala.reflect.runtime.universe._
package object EventHandler {
  import grammar._
  import typeable._

  trait Event {
    val amount: Double
  }

  case class spendEvent(amount: Double, Year: Int) extends Event

  type dep = Events with Consumption with Counter

  case class Events() extends model[Events with Consumption with Counter, Events] with TerminalType[Seq[Event]] {
    val formula: String = ""
    override def iterate(src: dataset[dep]): Option[Events] =
      for {
        consumptionRun <- src.calc[Consumption].calc[Consumption]//.fetch[Consumption]
        consumptionModel <- consumptionRun.fetch[Consumption]
        currEvents <- src.fetch[Events]
      } yield new Events {
        override val value: Seq[Event] = consumptionModel.value ++ currEvents.value
        override val formula: String = currEvents.formula + consumptionModel.value
          .map(e => s" + ${e.amount}")
          .foldLeft("")(_ + _)
      }
    //set initial value
    override val value: Seq[Event] = Seq()
  }
}

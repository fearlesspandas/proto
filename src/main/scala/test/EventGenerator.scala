package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
import src.main.scala.test.Consumption.{Consumption, Counter}

object EventGenerator{

  type dep = Events with Consumption with Counter
  case class EventGenerator() extends model[dep,EventGenerator] with TerminalType[Seq[Event]] {
    override def iterate(src: dataset[dep]): Option[EventGenerator] = for {
      consumptionModel <-src.calcFullContext[Consumption].fetch[Consumption]
    }
      yield new EventGenerator {
      override val value: Seq[Event] = consumptionModel.value
    }

    override val value: Seq[Event] = Seq()
    val formula:String = ""
  }
}

package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
import src.main.scala.test.Consumption.Consumption
object EventGenerator{
  type dep = Events with Consumption
  case class EventGenerator() extends model[dep,EventGenerator] with TerminalType[Seq[Event]] {
    override def iterate(src: dataset[dep]): EventGenerator = new EventGenerator {
      override val value: Seq[Event] = {
        val currEvents = src.fetch[Events].get.value
        val consumptionevents = src.calc[Consumption].fetch[Consumption].get.value
        currEvents ++ consumptionevents
      }
    }

    override val value: Seq[Event] = Seq()
  }
}

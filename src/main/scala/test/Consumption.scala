package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
object Consumption{
  type dep = Events
  var counter  = 0
  case class Consumption() extends model[dep,Consumption] with TerminalType[Seq[Event]] {
    override def iterate(src: dataset[dep]): Consumption = {
      counter = counter + 1
      new Consumption {
      override val value: Seq[Event] = Seq( spendEvent(counter))
    }
    }
    override val value: Seq[Event] = Seq()
  }
}

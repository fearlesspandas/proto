package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
object Consumption{
  type dep = Events
  case class Consumption() extends model[dep,Consumption] with TerminalType[Seq[Event]] {
    override def iterate(src: dataset[dep]): Consumption = new Consumption {
      override val value: Seq[Event] = Seq( spendEvent(5))
    }
    override val value: Seq[Event] = Seq()
  }
}

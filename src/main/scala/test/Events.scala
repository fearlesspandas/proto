package src.main.scala.test

import Typical.core.{grammar, typeable}
import scala.reflect.runtime.universe._
package object EventHandler{
  import grammar._
  import typeable._

  trait Event{
    val amount:Double
  }

  case class spendEvent(amount:Double,Year:Int) extends Event

  type dep = Events

  case class Events() extends model[Events,Events] with TerminalType[Seq[Event]] {
    val formula:String = ""
    override def iterate(src: dataset[dep]): Option[Events] = for{
      events <- src.fetch[Events]
    }yield events
    //set initial value
    override val value:Seq[Event] = Seq()
    def addEvents(events:Seq[Event]):Events = {
      val currevents = this.value
      val currformula = this.formula
      new Events {
        override val value: Seq[Event] = currevents ++ events
        override val formula: String = currformula + events.map(e => s" + ${e.amount}").foldLeft("")(_ + _)
      }
    }
  }
}

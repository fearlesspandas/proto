package test

import java.time.LocalDate

import Typical.core.grammar
import Typical.core.dataset._
import Consumption._
import Account._
package object EventHandler {
  import grammar._

  trait Event extends ::[Event]{
    val amount: Double
  }

  case class spendEvent(amount: Double, accountid: Long,date:LocalDate) extends Event
  case class depositEvent(amount: Double, accountid: Long,date:LocalDate) extends Event

  type dep = EventStore  with Accounts

  trait EventStore extends (EventStore with Accounts ==> EventStore){
    val value:Seq[Event]
    val formula:String
    def addEvents(events:Event*):dataset[EventStore]
  }
   case class Events(
                      value:Seq[Event]
                   ) extends EventStore{
     val formula = value.map(e => s" + ${e.amount}").foldLeft("")(_ + _)
    override def apply(src: dataset[dep]): dataset[EventStore] =
      for {
        consumptionModel <- src.<-+[Consumption]
      } yield Events(
          consumptionModel ++ value
        )
     def addEvents(events: Event*):dataset[EventStore] =
       Events(
        events.toSeq ++ this.value
      )
  }
  import scala.reflect.runtime.universe.TypeTag
  implicit class EventGrammar[A<:EventStore with Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def runEventLoop:dataset[A] = src.+->[EventStore]
  }
}

package src.main.scala.test
import Typical.core.grammar._
import EventHandler._
import Typical.core.dataset._
object Consumption {
  type EventDeps = EventStore with Counter
  type ImplicitEvents = EventStore
  type dep = EventDeps with ImplicitEvents
  type ConsumptionType = dep model Consumption
  case class Consumption(val value:Seq[Event]) extends ConsumptionType with produces[Seq[Event]]{
    override def apply(src: dataset[dep]): dataset[Consumption] =
      for {
        counter <- src.fetch[Counter]
        thing <- src.fetch[ImplicitEvents]
      } yield {
        println(thing.value)
        Consumption(Seq(spendEvent(counter * 2, counter)))
      }
  }
  case class Counter(value:Int) extends index[Counter] with produces[Int]{
    override def apply(): dataset[Counter] = Counter (value + 1)
  }
}


package src.main.scala.test
import Typical.core.grammar._
import EventHandler._
import Typical.core.dataset._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.runner.ProgramDependencies
import test.SpendEvents.SpendEvents
object Consumption {
  type EventDeps = EventStore with Counter
  //type implicitSpendEvents = implicitModel[Events,SpendEvents]
  type ImplicitEvents = EventStore//EventDeps model Events
  type dep = EventDeps with ImplicitEvents
  type ConsumptionType = dep model Consumption
  case class Consumption(val value:Seq[Event]) extends ConsumptionType {
    override def iterate(src: dataset[dep]): dataset[Consumption] =
      for {
        counter <- src.fetch[Counter]
        thing <- src.fetch[ImplicitEvents]
      } yield {
        println(thing.toString)
        Consumption(Seq(spendEvent(counter.value * 2, counter.value)))
      }
  }
  case class Counter(value:Int) extends model[Counter, Counter]{
    override def iterate(src: dataset[Counter]): dataset[Counter] =
      for {
        counter <- src.fetch[Counter]
      } yield Counter (counter.value + 1)
  }
}


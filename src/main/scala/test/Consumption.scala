package src.main.scala.test
import Typical.core.grammar._
import EventHandler._
import Typical.core.dataset._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.runner.ProgramDependencies
object Consumption {
  type MyTrait = model[_,Consumption]
  type progdep = ProgramDependencies
  type dep = model[
    Events with Consumption with Counter,
    Events
  ] with Counter  with MyTrait
  case class Consumption(value:Seq[Event]) extends model[dep, Consumption]{
    override def iterate(src: dataset[dep]): dataset[Consumption] =
      for {
        counter <- src.fetch[Counter]
        thing <- src.fetch[MyTrait]
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


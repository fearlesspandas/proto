package src.main.scala.test
import Typical.core.grammar._
import EventHandler._
import Typical.core.dataset._
import src.main.scala.test.runner.ProgramDependencies
object Consumption {
  type progdep = ProgramDependencies
  type dep = Events with Counter
  case class Consumption( value:Seq[Event]) extends model[dep, Consumption]{
    override def iterate(src: dataset[dep]): dataset[Consumption] =
      for {
        counter <- src.fetch[Counter]
      } yield Consumption(Seq(spendEvent(counter.value * 2, counter.value)))
  }
  case class Counter(value:Int) extends model[Counter, Counter]{
    override def iterate(src: dataset[Counter]): dataset[Counter] =
      for {
        counter <- src.fetch[Counter]
      } yield Counter (counter.value + 1)
  }
}

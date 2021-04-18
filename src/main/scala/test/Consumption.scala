package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
import Grammar.Calc
import src.main.scala.test.runner.ProgramDependencies
import src.main.scala.test.runner.Sim
object Consumption {
  type progdep = ProgramDependencies
  type dep = Events with Counter //with Calc[progdep, Counter]
  case class Consumption(override val value:Seq[Event]) extends model[dep, Consumption] with produces[Seq[Event]] {
    override def iterate(src: dataset[dep]): Option[Consumption] =
      for {
        counter <- src.fetch[Counter]
      } yield new Consumption(Seq(spendEvent(counter.value * 2, counter.value)))
  }
  case class Counter(override val value:Int) extends model[Counter, Counter] with produces[Int] {
    override def iterate(src: dataset[Counter]): Option[Counter] =
      for {
        counter <- src.fetch[Counter]
        curr = counter.value
      } yield Counter (curr + 1)
  }
}

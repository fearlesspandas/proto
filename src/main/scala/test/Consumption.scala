package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
import Grammar.Calc
import src.main.scala.test.EventGenerator.EventGenerator
import src.main.scala.test.runner.ProgramDependencies
import src.main.scala.test.runner.Sim
object Consumption {
  type progdep = ProgramDependencies
  type dep = Events with Counter //with Calc[progdep, Counter]
  case class Consumption() extends model[dep, Consumption] with TerminalType[Seq[Event]] {
    override def iterate(src: dataset[dep]): Option[Consumption] =
      for {
        counter <- src.fetch[Counter]
      } yield new Consumption {
        override val value: Seq[Event] = Seq(spendEvent(counter.value * 2, counter.value))
      }
    override val value: Seq[Event] = Seq()

    override def withContext(ctx: contexttype): dataset[Consumption] = {
      val currvalue = this.value
      new Consumption {
        override val value = currvalue
        override val context: contexttype = ctx
      }
    }
  }
  case class Counter() extends model[Counter, Counter] with TerminalType[Int] {
    override def iterate(src: dataset[Counter]): Option[Counter] =
      for {
        counter <- src.fetch[Counter]
        curr = counter.value
      } yield new Counter {
        override val value = curr + 1
      }

    override val value: Int = -1
  }
}

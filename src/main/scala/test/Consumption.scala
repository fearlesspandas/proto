package src.main.scala.test
import Typical.core.grammar._
import Typical.core.typeable._
import EventHandler._
import Grammar.Calc
import src.main.scala.test.runner.ProgramDependencies
import src.main.scala.test.runner.Sim
case class NoConsumptionFound(message:String) extends Error(message)
case class NoCounterFound(message:String) extends Error(message)
object Consumption {
  type progdep = ProgramDependencies
  type dep = Events with Counter //with Calc[progdep, Counter]
  case class Consumption(override val value:Seq[Event]) extends FinalModel[dep, Consumption] with produces[Seq[Event]] with failsWith[NoConsumptionFound]{
    override def iterate(src: dataset[dep]): Option[Consumption] =
      for {
        counter <- src.fetch[Counter]
      } yield new Consumption(Seq(spendEvent(counter.value * 2, counter.value)))

    override val err: NoConsumptionFound = NoConsumptionFound("No value was found for Consumption")
  }
  case class Counter(override val value:Int) extends FinalModel[Counter, Counter] with produces[Int] with failsWith[NoCounterFound]{
    override def iterate(src: dataset[Counter]): Option[Counter] =
      for {
        counter <- src.fetch[Counter]
        curr = counter.value
      } yield Counter (curr + 1)

    override val err: NoCounterFound = NoCounterFound("No value found for Counter")
  }
}

package test
import Typical.core.grammar._
import Typical.core.dataset._
import Account._
import test.Event.Event
object Consumption {
  type dep = Accounts
  type ConsumptionType = dep ==> Consumption
  case class Consumption(val value:Seq[Event]) extends ConsumptionType with produces[Seq[Event]]{
    override def apply(src: dataset[dep]): dataset[Consumption] =
      for {
        counter <- src.accounts
      } yield {
                Consumption(Seq())
      }
  }
  case class Counter(value:Int) extends index[Counter] with produces[Int]{
    override def apply(): dataset[Counter] = Counter (value + 1)
  }
}


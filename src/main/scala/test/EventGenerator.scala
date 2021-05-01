//package src.main.scala.test
//import Typical.core.grammar._
//import Typical.core.typeable._
//import EventHandler._
//import Grammar.Calc
//import src.main.scala.test.Consumption.Consumption
//import src.main.scala.test.Consumption.Counter
//import src.main.scala.test.runner.ProgramDependencies
//import src.main.scala.test.runner.Sim
//
////object EventGenerator {
////  type progdep = Events with EventGenerator with Consumption with Counter with Sim
////  type dep = Events with Consumption with Counter with Calc[Events with EventGenerator with Consumption with Counter with Sim, Counter]
////  case class EventGenerator() extends model[dep, Events] with produces[Seq[Event]] {
////    override def iterate(src: dataset[dep]): Option[Events] =
////      for {
////        consumptionRun <- src.calc[Consumption]//.fetch[Consumption]
////        consumptionModel <- consumptionRun.fetch[Consumption]
////        currEvents <- src.fetch[Events]
////      } yield new Events {
////        override val value: Seq[Event] = consumptionModel.value ++ currEvents.value
////        override val formula: String = currEvents.formula + consumptionModel.value
////          .map(e => s" + ${e.amount}")
////          .foldLeft("")(_ + _)
////      }
////
////
////    override val value: Seq[Event] = Seq()
////    val formula: String = ""
////  }
////}

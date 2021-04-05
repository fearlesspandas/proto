package src.main.scala.test

import Grammar.Calc
import Typical.core._
import src.main.scala.test.Consumption.{Consumption, Counter}
import src.main.scala.test.EventGenerator.EventGenerator
import src.main.scala.test.EventHandler._
import test.SpendEvents.SpendEvents

object runner {

  import grammar._
  import typeable._

  import scala.reflect.runtime.universe._

  type ProgramDependencies = Events with EventGenerator with Consumption with Counter with Sim //with Calc[Events with Counter,Counter]
  val startData = Map[Any, dataset[_]]()
    .register[Events](new Events)
    .register[EventGenerator](new EventGenerator)
    .register[Consumption](new Consumption)
    .register[Counter](new Counter)
    .register[Sim](new Sim)
  def main(args: Array[String]): Unit = {
    val dat = (new Prog).value.flatMap[Prog].flatMap[Prog].flatMap[Prog]
    //val res = dat.calc[SpendEvents].fetch[SpendEvents].get.value(1)
    val formula = dat.fetch[Events].get.formula
    println(dat.context.valueView())
    println(s"formula:${formula}")
  }
  
  //trait balanced[From <: model[ProgramDependencies, From] with TerminalType[Double], To <: model[ProgramDependencies, To] with TerminalType[Double], self <: balanced[_, _, self]] extends directive[ProgramDependencies with From with To, self]
  def prog(src:dataset[ProgramDependencies]):Option[Prog] = for {
    eventgenerator <- src.calc[Counter].map[EventGenerator].fetch[EventGenerator]
    thing = src.calc[Counter].map[EventGenerator]
    newevents = eventgenerator.value
    currentevents <- src.fetch[Events]
    updatedEvents = currentevents.addEvents(newevents)
  }yield new Prog {
    override val value: dataset[ProgramDependencies] = {
      val t = src.include[Events](updatedEvents).calc[Counter].calc[Counter].calc[Counter]
      val s = src.calc[Counter].map[EventGenerator].calc[Counter].map[EventGenerator].map[EventGenerator]//.fetch[EventGenerator]
      t
    }
  }
  def prog2(src:dataset[ProgramDependencies]):Option[Prog] = for {
    eventgenerator <- src.calc[Counter].map[EventGenerator].fetch[EventGenerator]
    newevents = eventgenerator.value
    currentevents <- src.fetch[Events]
    updatedEvents = currentevents.addEvents(newevents)

  }yield new Prog {
    override val value: dataset[ProgramDependencies] = src.include[Events](currentevents)
  }

  case class Sim() extends axiom[Sim] with TerminalType[String] {
    override val value: String = "all"
  }
  case class Prog() extends directive[ProgramDependencies, Prog]{
    override val value: dataset[ProgramDependencies] = data[ProgramDependencies](startData)
    override def iterate(src: dataset[ProgramDependencies]): Option[Prog] = for{
      sim <- src.fetch[Sim]
      simType = sim.value
      res <- (if (simType == "Empty") prog2(src) else prog(src))
    }yield res

  }
}
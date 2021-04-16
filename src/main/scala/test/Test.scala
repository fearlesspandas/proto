package src.main.scala.test

import Grammar.Calc
import Typical.core._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.Consumption.Counter
import src.main.scala.test.EventGenerator.EventGenerator
import src.main.scala.test.EventHandler._
import test.SpendEvents.SpendEvents

object runner {

  import grammar._
  import typeable._

  type ProgramDependencies = Events with Consumption with Counter with Sim //with Calc[Events with Counter,Counter]
  val startData = Map[Any, dataset[_]]()
    .register[Events](new Events)
    .register[EventGenerator](new EventGenerator)
    .register[Consumption](new Consumption)
    .register[Counter](new Counter)
    .register[Sim](new Sim)
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println("Processing")
    val dat = (new Prog).value
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]
      .flatMap[Prog]

    //(0 to 100).foldLeft((new Prog).value)((a,_) => a.flatMap[Prog])
    //val res = dat.calc[SpendEvents].fetch[SpendEvents].get.value(1)
    val formula = dat.fetch[Events].get.formula
    println(dat.context.valueView())
    println(s"formula:${formula}")
    val end = System.currentTimeMillis()
    println(s"time elapsed:${end - start} milliseconds")
  }

  def prog(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      newEvents <- src.calcT[Counter].calc[Events]
    } yield new Prog {
      override val value: dataset[ProgramDependencies] = newEvents.flatMap[Prog2]
    }
  def prog2(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      eventgenerator <- src.calc[Counter]

    } yield new Prog {
      override val value: dataset[ProgramDependencies] = src//.include[Events](currentevents)
    }

  case class Prog2() extends directive[Events with Consumption with Counter,Prog2] {
    override def iterate(src: dataset[Events with Consumption with Counter]): Option[Prog2] = for{
      x <- src.calc[Counter]
    }yield new Prog2 {
      override val value = x
    }

    override val value: dataset[Events with Consumption with Counter] = data[Events with Consumption with Counter](Map[Any,dataset[_]]().register[Events](new Events))
  }
  case class Sim() extends axiom[Sim] with produces[String] {
    override val value: String = "all"
  }
  case class Prog() extends directive[ProgramDependencies, Prog] {
    override val value: dataset[ProgramDependencies] = data[ProgramDependencies](startData)
    override def iterate(src: dataset[ProgramDependencies]): Option[Prog] =
      for {
        sim <- src.fetch[Sim]
        simType = sim.value
        res <- (if (simType == "Empty") prog2(src) else prog(src))
      } yield res

  }
}

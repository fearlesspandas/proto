package src.main.scala.test

import Grammar.Calc
import Typical.core._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.Consumption.Counter
import src.main.scala.test.EventHandler._
import test.SpendEvents.SpendEvents

object runner {

  import grammar._
  import typeable._

  type ProgramDependencies = Events with Consumption with Counter with Sim //with Calc[Events with Counter,Counter]
  val startData = Map[Any, dataset[_]]()
    .register[Events](new Events(Seq(),""))
    .register[Consumption](new Consumption(Seq()))
    .register[Counter](Counter(-1))
    .register[Sim](new Sim)
    .register[Prog](new Prog(null))
    .register[Prog2](new Prog2(null))
  case class thingy(a:Int)
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println("Processing")
    val dat = data[ProgramDependencies](startData)
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
    } yield new Prog ( newEvents.flatMap[Prog])
  def progf(src:dataset[ProgramDependencies]):Option[dataset[ProgramDependencies]] = for {
    newEvents <- src.calcT[Counter].calc[Events]
  }yield newEvents
  def prog2(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      eventgenerator <- src.calc[Counter]

    } yield new Prog (
      src//.include[Events](currentevents)
    )

  case class Prog2(override val value: dataset[Events with Consumption with Counter]) extends directive[Events with Consumption with Counter,Prog2] {
    override def apply(value: dataset[Events with Consumption with Counter]): Prog2 = Prog2(value)
    override def next(src: dataset[Events with Consumption with Counter]): Option[dataset[Events with Consumption with Counter]] = for{
      x <- src.calc[Counter]
    } yield x
  }
  case class Sim() extends axiom[Sim,String]{
    override val value: String = "all"

    override def withValue(newVal: String): Sim = new Sim{
      override val value  = newVal
    }
  }
  case class Prog(override val value: dataset[ProgramDependencies]) extends directive[ProgramDependencies, Prog] {
    override def apply(value: dataset[ProgramDependencies]): Prog = Prog(value)
    override def next(src: dataset[ProgramDependencies]): Option[dataset[ProgramDependencies]] = for {
      res <- progf(src)
    }yield res
  }
}

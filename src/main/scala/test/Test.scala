package src.main.scala.test

import Grammar.Calc
import Typical.core._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.Consumption.Counter
import src.main.scala.test.EventHandler._

object runner {

  import grammar._
  import typeable._

  type ProgramDependencies = Events with Consumption with Counter with Sim //with Calc[Events with Counter,Counter]
  val startData = Map[Any, dataset[_]]()
    .register[Events](Events(Seq(),""))
    .register[Consumption](new Consumption(Seq()))
    .register[Counter](Counter(-1))
    .register[Sim](Sim("all"))
    .register[Prog](Prog(null))
    .register[Prog2](Prog2(null))

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
    val end = System.currentTimeMillis()
    //(0 to 100).foldLeft((new Prog).value)((a,_) => a.flatMap[Prog])
    //val res = dat.calc[SpendEvents].fetch[SpendEvents].get.value(1)
    val formula = dat.fetch[Events].get.formula
    println(dat.context.valueView())
    println(s"formula:${formula}")

    println(s"time elapsed:${end - start} milliseconds")
  }

  def prog(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      newEvents <- src.calcT[Counter].calc[Events]
    } yield  Prog ( newEvents.flatMap[Prog])

  def progf(src:dataset[ProgramDependencies]):Option[dataset[ProgramDependencies]] = for {
    newEvents <- src.calcT[Counter].calc[Events]
  }yield newEvents


  def prog2(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      _ <- src.calc[Counter]
    } yield Prog (src)
  type prog2deps = Events with Consumption with Counter
  case class Prog2(override val value: dataset[prog2deps]) extends directive[prog2deps,Prog2] {
    override def apply(value: dataset[prog2deps]): Prog2 = Prog2(value)
    override def next(src: dataset[prog2deps]): Option[dataset[prog2deps]] = for{
      x <- src.calc[Counter]
    } yield x
  }
  case class Sim(value:String) extends axiom[Sim,String]{
    override def withValue(newVal: String): Sim = Sim(newVal)
  }
  case class Prog(override val value: dataset[ProgramDependencies]) extends directive[ProgramDependencies, Prog] {
    override def apply(value: dataset[ProgramDependencies]): Prog = Prog(value)
    override def next(src: dataset[ProgramDependencies]): Option[dataset[ProgramDependencies]] = for {
      res <- progf(src)
    }yield res
  }
}

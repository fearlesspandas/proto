package src.main.scala.test

import Typical.core._
import src.main.scala.test.Consumption.{Consumption, Counter, EventDeps}
import src.main.scala.test.EventHandler._
import test.SpendEvents.SpendEvents

object runner {

  import grammar._
  import dataset._
  val starterConsumption:Consumption = new Consumption(Seq())
  val starterEvents :Events = Events(Seq(),"")
  type ProgramDependencies = Events with Consumption with Counter
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println("Processing")
    val dat = data[ProgramDependencies]()
      .include(starterConsumption)
      .include(Counter(-1))
      .include(Prog())
      .include[EventStore,Events](starterEvents)
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
      .run[Prog]
    val end = System.currentTimeMillis()
    println(dat)
    println(s"time elapsed:${end - start} milliseconds")
  }


  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def iterate(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] ={
      src.calc[Counter].calc[Events]
    }
  }








}

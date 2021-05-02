package src.main.scala.test

import Typical.core._
import src.main.scala.test.Consumption.{Consumption, Counter, EventDeps}
import src.main.scala.test.EventHandler._
import test.SpendEvents.SpendEvents

object runner {

  import grammar._
  import dataset._
  val starterConsumption:Consumption = new Consumption(Seq())
  val starterEvents :Events = new Events(Seq(),"")


  type ProgramDependencies = EventStore with Consumption with Counter
  val startData = Map[idtype, dataset[_]]()
    .register[Consumption](starterConsumption)
    .register[Counter](Counter(-1))
    .register[Events](starterEvents)
    .register[Prog](Prog())
    .register[SpendEvents](SpendEvents(Seq()))


  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println("Processing")
    val ev = buildId[EventDeps]
    println(ev)
    //println(buildIdString(ev))
    println(buildId[Events with Counter],buildId[Events with Consumption])
    val dat1 = data[ProgramDependencies](startData).bind[EventStore,Events]//.include[EventStore](starterEvents)
      val dat = dat1.withContext(dat1.context)
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
        //val dat2 = remove(dat,Events)
    val end = System.currentTimeMillis()
    println(dat.context)
    println(s"time elapsed:${end - start} milliseconds")
  }


  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def iterate(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = src.calc[Counter].calc[EventStore]
  }








}

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
    val dat = data[Counter]()
      .include(starterConsumption)
      .include(Counter(-1))
      .include(Prog())
      .include[EventStore,Events](starterEvents)
      .include[CycleState,One](One())
    runProg(dat)
//      .calc[CycleState]
//      .calc[CycleState]
//      .calc[CycleState]
//      .run[One]
//      .calc[CycleState]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
//      .run[Prog]
    val end = System.currentTimeMillis()
    println(dat.fetch[CycleState])
    println(dat.fetch[Events])
    println(s"time elapsed:${end - start} milliseconds")
  }


  def runProg(src:dataset[ProgramDependencies]):dataset[ProgramDependencies] = {
    Thread.sleep(1000)
    runProg(src.run[Prog])
  }

  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] ={
      src.calc[Counter].calc[Events]
    }

  }



trait CycleState extends model[CycleState,CycleState]

  case class One() extends CycleState {
    override def apply(src: dataset[CycleState]): dataset[CycleState] = Two()
  }
  case class Two() extends CycleState{
    override def apply(src: dataset[CycleState]): dataset[CycleState] = Three()
  }
  case class Three() extends CycleState {
    override def apply(src: dataset[CycleState]): dataset[CycleState] = One()
  }



}

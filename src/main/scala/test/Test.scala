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
      .include(Compound(13000))
      .include(Rand(0))
      .calc[Compound]
      .calc[Compound]
      .calc[Compound]
      .calc[Compound]
      .calc[Compound]
    //runProg(dat)
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
//      .run[Prog],rand2.value
    val end = System.currentTimeMillis()
    println(dat.fetch[CycleState])
    println(dat.fetch[Compound].asInstanceOf[Compound].value)
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

  case class Rand(value:Double) extends index[Rand] with produces[Double]{
    override def apply(): dataset[Rand] = Rand(scala.util.Random.nextDouble())
  }
  case class Compound(value:Double) extends model[Compound with Rand,Compound]{
    override def apply(src: dataset[Compound with Rand]): dataset[Compound] = for{
      rand <- src.derive[Rand]
    }yield{
      println(rand.value)
      Compound(value* 1.7 * 10 * rand)
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

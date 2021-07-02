package test

import test.SpendWildly.SpendWildly

import scala.annotation.implicitNotFound


object runner {
  import Typical.core._
  import Consumption._
  import EventHandler._
  import test.Account._
  import test.GrowAccounts._
  import SpendWildly._
  import grammar._
  import dataset._

  val starterConsumption:Consumption = new Consumption(Seq())
  val starterEvents :Events = Events(Seq())
  type ProgramDependencies = EventStore with Consumption with Counter with Accounts with Compound with Rand with AccountRates

  val start = System.currentTimeMillis()
  println("Processing")
  val dat = data[Counter]()
    .include(starterConsumption)
    .include(Counter(1))
    .include(SpendEvents.SpendEvents(Seq()))
    .include(Prog())
    .include[EventStore,Events](starterEvents)
    .include[CycleState,One](One())
    .include(Compound(13000))
    .include(Rand(0))
    .include(Accounts(Seq(CheckingAccount(1,100),BokerageAccount(2,10000)),Seq()))
    .include(AccountRates())
    .run[Prog]

  val end = System.currentTimeMillis()
  //println(dat.accounts.getValue)
  println(s"time elapsed:${end - start} milliseconds")
  def main(args: Array[String]): Unit = {
    dat.console()
  }

  @implicitNotFound("Prog doesnt have its deps")
  case class Prog() extends ==>[ProgramDependencies, ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] ={

      //iterate year
      //grow accounts
      //
      src.growAccounts.growAccounts.growAccounts.growAccounts
    }
  }

  case class Rand(value:Double) extends index[Rand] with produces[Double]{
    override def apply(): dataset[Rand] = Rand(scala.util.Random.nextDouble())
  }
  case class Compound(value:Double) extends ==>[Compound with Rand,Compound]{
    override def apply(src: dataset[Compound with Rand]): dataset[Compound] = for{
      rand <- src.derive[Rand]
    }yield{
      println(rand.value)
      Compound(value* 1.7 * 10 * rand)
    }
  }

trait CycleState extends ==>[CycleState,CycleState]

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

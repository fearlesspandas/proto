package test


object runner {
  import Typical.core._
  import src.main.scala.test.Consumption.{Consumption, Counter}
  import src.main.scala.test.EventHandler._
  import test.Account._
  import grammar._
  import dataset._

  val starterConsumption:Consumption = new Consumption(Seq())
  val starterEvents :Events = Events(Seq(),"")
  type ProgramDependencies = EventStore with Consumption with Counter with Accounts with Compound with Rand with AccountRates

  val start = System.currentTimeMillis()
  println("Processing")
  val dat = data[Counter]()
    .include(starterConsumption)
    .include(Counter(1))
    .include(Prog())
    .include[EventStore,Events](starterEvents)
    .include[CycleState,One](One())
    .include(Compound(13000))
    .include(Rand(0))
    .include(Accounts(Seq(CheckingAccount(1,100))))
    .include(AccountRates())
    .include(GrowAccounts)
    .include(SpendWildly())
    .iter[Compound]
    .iter[Compound]
    .iter[Compound]
    .iter[Compound]
    .iter[Compound]
    .run[SpendWildly]
    .run[SpendWildly]
    .run[SpendWildly]
    .run[SpendWildly]
    .growAccounts
    .growAccounts
    .run[Prog]

  val end = System.currentTimeMillis()
  println(dat.accounts.getValue)
  val t = data[Accounts]().getValue//.include(Accounts(Seq()))
  println(t)
  println(dat)
  println(s"time elapsed:${end - start} milliseconds")
  def main(args: Array[String]): Unit = {
    dat.console()
  }


  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] ={
      src.iter[Counter].iter[EventStore]
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

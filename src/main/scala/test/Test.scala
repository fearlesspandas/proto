package test


object runner {
  import Typical.core._
  import src.main.scala.test.Consumption.{Consumption, Counter, EventDeps}
  import src.main.scala.test.EventHandler._
  import test.Account._
  import test.AccountRates
  import test.SpendEvents.SpendEvents
  import grammar._
  import dataset._
  def init() = {
    import Typical.core._
    import src.main.scala.test.Consumption.{Consumption, Counter, EventDeps}
    import src.main.scala.test.EventHandler._
    import test.Account._
    import test.AccountRates
    import test.SpendEvents.SpendEvents
    import grammar._
    import dataset._
  }
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
    .include(new Accounts(Seq(CheckingAccount(1,100))))
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
    // dat.transfer()
    //
    .run[Prog]

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
  println(dat.accounts.asInstanceOf[Accounts].value)
  // println(dat.fetch[CycleState])
  //   println(dat.fetch[Compound].asInstanceOf[Compound].value)
  //    println(dat.fetch[Accounts].asInstanceOf[Accounts].value)
  //    println(dat.spend(CheckingAccount(2,0),10).fetch[Accounts].asInstanceOf[Accounts].value)
  println(s"time elapsed:${end - start} milliseconds")
  def main(args: Array[String]): Unit = {
    dat.console()
  }



//  def runProg(src:dataset[ProgramDependencies]):dataset[ProgramDependencies] = {
//    val cmd = scala.io.StdIn.readLine()
//    println(src.values.filter(_.toString.toUpperCase().contains(cmd.toUpperCase())))
//    runProg(src.run[Prog])
//  }

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

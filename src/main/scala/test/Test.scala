package src.main.scala.test

import Grammar.Calc
import Typical.core._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.Consumption.Counter
import src.main.scala.test.EventHandler._
import src.main.scala.test.runner.Monady.{Monady, NoRes, Res}

object runner {

  import grammar._
  import typeable._

  type ProgramDependencies = Events with Consumption with Counter with Sim //with andThen[Counter,Counter] //with Calc[Events with Counter,Counter]
  val startData = Map[idtype, dataset[_]]()
    .register[Events](Events(Seq(),""))
    .register[Consumption](new Consumption(Seq()))
    .register[Counter](Counter(-1))
    .register[Sim](Sim("all"))
    .register[Prog](Prog(null))
    .register[Prog2](Prog2(null))
    //.filterKeys(k => !(k== Counter(-1).id))
    //.filterNot(p => p._2.isInstanceOf[Counter])
    //.register(andThen(null))

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(run())
    println("Processing")
    val dat1 = data[ProgramDependencies](startData)
      val dat = dat1.withContext(dat1.context.filterNot(p => p._2.isInstanceOf[Prog]))
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
      .flatCalc[Prog]
    val end = System.currentTimeMillis()
    //(0 to 100).foldLeft((new Prog).value)((a,_) => a.flatMap[Prog])
    //val res = dat.calc[SpendEvents].fetch[SpendEvents].get.value(1)
    val formula = dat.fetch[Events].get.formula
    println(dat.get.context.valueView())
    println(s"formula:${formula}")

    println(s"time elapsed:${end - start} milliseconds")
  }

  def prog(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      newEvents <- src.calcT[Counter].calc[Events].flatCalc[Prog]
    } yield  Prog (newEvents)

  def progf(src:dataset[ProgramDependencies]):Option[dataset[ProgramDependencies]] = for {
    newEvents <- src.calcT[Counter].calcT[Events]
  }yield newEvents


  def prog2(src: dataset[ProgramDependencies]): Option[Prog] =
    for {
      _ <- src.calc[Counter]
    } yield Prog (src)

  type prog2deps = Events with Consumption with Counter
  case class Prog2(override val value: dataset[prog2deps]) extends modelUnion[prog2deps,Prog2]{
    override def apply(value: dataset[prog2deps]): Prog2 = Prog2(value)
    override def next(src: dataset[prog2deps]): Option[dataset[prog2deps]] = for{
      x <- src.calc[Counter]
    } yield x
  }
  case class Sim(value:String) extends axiom[String,Sim]{
    override def withValue(newVal: String): Sim = Sim(newVal)
  }
  case class RegisterProgError(message:String) extends Error(message)
  case class Prog(override val value: dataset[ProgramDependencies]) extends modelUnion[ProgramDependencies, Prog] with failsWith [RegisterProgError]{
    override def apply(value: dataset[ProgramDependencies]): Prog = Prog(value)
    override def next(src: dataset[ProgramDependencies]): Option[dataset[ProgramDependencies]] = for {
      res <- progf(src)
    }yield res

    override val err: RegisterProgError = RegisterProgError(s"No value for ${this.id} found, make sure ${this.id} is registered in the src context")
  }







object Monady{
  def apply[A<:Monady[_]](a:A):Monady[A] = Res[A](a)
  sealed trait Monady[+A<:Monady[_]]{
    //def flatMap[B<:Monady[_]](f:A => Monady[B]):Monady[B] = null//f(this)
    def iter[dep<:Monady[_]](src:Monady[dep]):Monady[A] = null
    def flatMap[B<:Monady[_]](src:Monady[B]):Monady[B with A] = null
    def map[B<:Monady[_]](f:Monady[A] => B):Monady[B] = apply[B](f(this))
  }
  case class Res[A<:Monady[_]](value:A) extends Monady[A]
  case object NoRes extends Monady[Nothing]
}

  def run() = for {
     one <- Res(NoRes)
    two <- Res(NoRes)
  }yield (two)


}

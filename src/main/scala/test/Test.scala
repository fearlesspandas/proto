package src.main.scala.test

import Typical.core._
import src.main.scala.test.Consumption.{Consumption, Counter, MyTrait}
import src.main.scala.test.EventHandler._

object runner {

  import grammar._
  import dataset._

  type ProgramDependencies = Events with Consumption with Counter
  val startData = Map[idtype, dataset[_]]()
    .register[Consumption](new Consumption(Seq()))
    .register[Counter](Counter(-1))
    .bind[MyTrait,Events](Events(Seq(),""))
    .register[Events](Events(Seq(),""))
    .register[Prog](Prog())

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println("Processing")
    val dat1 = data[ProgramDependencies](startData)
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
    println(buildId[MyTrait])
    val end = System.currentTimeMillis()
    println(dat.context)
    println(s"time elapsed:${end - start} milliseconds")
  }


  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def iterate(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = src.calc[Counter].calc[Events]
  }








}

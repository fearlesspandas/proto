package src.main.scala.test

import Typical.core._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.Consumption.Counter
import src.main.scala.test.EventHandler._

object runner {

  import grammar._
  import dataset._

  type ProgramDependencies = Events with Consumption with Counter
  val startData = Map[idtype, dataset[_]]()
    .register[Events](Events(Seq(),""))
    .register[Consumption](new Consumption(Seq()))
    .register[Counter](Counter(-1))
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
    val end = System.currentTimeMillis()
    println(dat.context)
    println(s"time elapsed:${end - start} milliseconds")
  }


  case class Prog() extends model[ProgramDependencies, ProgramDependencies] {
    override def iterate(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = src.calc[Counter].calc[Events]
  }








}

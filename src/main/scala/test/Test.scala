package src.main.scala.test

import Typical.core._
import EventHandler._
import src.main.scala.test.Consumption.Consumption
import src.main.scala.test.EventGenerator.EventGenerator
object runner {
  import grammar._
  import typeable._

  import scala.reflect.runtime.universe._
  type ProgramDependencies = Events with EventGenerator with Consumption
  case class Prog( ) extends model[Prog,Prog] with TerminalType[dataset[ProgramDependencies]] {
    override def iterate(src: dataset[Prog]): Prog = {
      val dat = src.fetch[Prog].get.value
      val newevents = dat.calc[EventGenerator].fetch[EventGenerator].get.value
      val currentevents = dat.fetch[Events].get
      val updatedEvents = currentevents.addEvents(newevents)
      new Prog{
        override val value: dataset[ProgramDependencies] = dat.include[Events](updatedEvents)
      }
    }

    override def withContext(ctx: contexttype): dataset[Prog] = 
    override val value: dataset[ProgramDependencies] = data[ProgramDependencies](Map[Any,dataset[_]]())
  }
  case class andThen[
    A<:model[A,A],
    B<:model[B,B]
  ](implicit
    atag:TypeTag[A],
    btag:TypeTag[B],
    alltag:TypeTag[andThen[A,B]]
   ) extends directive [A with B,andThen[A,B]] with TerminalType [dataset[A with B]]{
    override def iterate(src: dataset[A with B]): andThen[A,B] = new andThen[A,B] {
      override val value = src.calc[A].calc[B]
    }

    override val value: dataset[A with B] = null
  }

  def main(args:Array[String]):Unit = {
  val dat = data[Prog](Map()).calc[Prog].calc[Prog].fetch[Prog].get.value
    val res = dat.fetch[Events].get.value
    println(res)
  }
}
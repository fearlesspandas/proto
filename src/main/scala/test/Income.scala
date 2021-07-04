package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Date._
import EventHandler._

import scala.reflect.runtime.universe.TypeTag
object Income {
  //define incomes
  trait Income{
    val id:Long
    val amount:Double
    val startDate:Date
    val endDate:Date
  }
  case class ficaTaxableincome(id:Long,amount:Double, startDate:Date,endDate:Date) extends Income

  //defin income events
  trait IncomeEvent extends ::[IncomeEvent]{
    val incomeId:Long
    val amount:Double
    val date:LocalDate
  }
  case class taxableIncomeEvent(incomeId:Long,amount:Double,date:LocalDate) extends IncomeEvent{

  }


  type IncomeEventGenDeps = Incomes with Date
  case class IncomeEventGenerator(value:Seq[IncomeEvent]) extends
    (IncomeEventGenDeps ==> IncomeEventGenerator) with
    produces[Seq[IncomeEvent]]{
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[IncomeEventGenerator] = for{
      incomes <- src.incomes
      date <- src.currentDate
    }yield date match {
      case _:Month|_:Week =>
        val toBePaidOut = incomes.value.collect(
        {
          case i:Income if (scala.math.abs(date.getDayOfMonth - i.startDate.getDayOfMonth) < date.numberOfDays) => i
        }
      )
        IncomeEventGenerator(
          toBePaidOut.map(i => taxableIncomeEvent(i.id,i.amount,date))
        )
      case y:Year => ???
      case d:Day => ???
    }
  }


  case class Incomes(value:Seq[Income],eventLog:Seq[Event]) extends (Incomes ==> Incomes){
    override def apply(src: dataset[Incomes]): dataset[Incomes] = src.fetch[Incomes]
    private def apply(income:Income):dataset[Incomes] = {
      val incomeMapCurr = this.incomeMap
      val exists = incomeMap.get(income.id).isDefined
      lazy val updatedMap = incomeMap.updated(income.id,income)
      val newIncomeColl = if(exists) updatedMap.values.toSeq else value :+ income
      new Incomes(newIncomeColl,this.eventLog){
        lazy val incomeMap = if (incomeMapCurr != null) updatedMap else Map(income.id -> income)
      }
    }
    private[Income] def addEvent(events:Event*):dataset[Incomes] = new Incomes(this.value,events ++ this.eventLog)
    private lazy val incomeMap:Map[Long,Income] = value.map(a => a.id -> a).toMap

    def get(id:Long):Income = incomeMap(id)
    def update(acct:Income):dataset[Incomes] = apply(acct)
  }

  implicit class IncomeGrammar[A<:Incomes](src:dataset[A])(implicit taga:TypeTag[A]){
    def incomes:dataset[Incomes] = if(src.isInstanceOf[Incomes]) src else src.fetch[Incomes]
  }
  implicit class IncomeDateDrivenBehavior[A<:Incomes with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def generateIncomeEvents:dataset[IncomeEventGenerator] =
      src
        .includeIfNotPresent(IncomeEventGenerator(Seq()))
        .iter[IncomeEventGenerator]
  }

}

package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Date._
import EventHandler._
import Account._
import scala.reflect.runtime.universe.TypeTag
object Income {
  type IncomeEventGenDeps = Incomes with Date
  //define incomes
  trait Income extends (IncomeEventGenDeps ==> Income) with produces[Seq[IncomeEvent]]{
    val id:Long
    val payableTo:Long
    val amount:Double
    val dateRange:DateRange
  }
  import scala.math.abs
  case class ficaTaxableincome(id:Long,amount:Double,payableTo:Long, dateRange:DateRange,eventLog:Seq[IncomeEvent] = Seq()) extends Income{
    override val value = eventLog
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[Income] =
    (for{
      date <- src.currentDate
      expandedDateRange <- (data[Date]().++(dateRange.start).++(dateRange)).<-+[DateRange]
      possibleDate <- expandedDateRange.findClosestPeriod(src)
    }yield {
      if( this.isActive(date)) {
        this.copy(eventLog = taxableIncomeEvent(this.id,this.payableTo,amount,possibleDate) +: eventLog)
      }
      else this
    })
    def preceeds(date:Date):Boolean = date.isBefore(dateRange.start)
    def halted(date:Date):Boolean = date.isAfter(dateRange.end)
    def isActive(date:Date):Boolean = !preceeds(date) && !halted(date)
  }

  //defin income events
  trait IncomeEvent extends ::[IncomeEvent]{
    val incomeId:Long
    val payableTo:Long
    val amount:Double
    val date:LocalDate
  }
  case class taxableIncomeEvent(incomeId:Long,payableTo:Long,amount:Double,date:LocalDate) extends IncomeEvent{

  }



  case class IncomeEventGenerator(value:Seq[IncomeEvent]) extends
    (IncomeEventGenDeps ==> IncomeEventGenerator) with
    produces[Seq[IncomeEvent]]{
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[IncomeEventGenerator] = for{
      incomes <- src.incomes
    }yield {
        val toBePaidOut = incomes.value.flatMap(i => {
         src.++(i).<-+[Income].getOrElse[Seq[IncomeEvent]](Seq())
        })
        IncomeEventGenerator(
          toBePaidOut
        )
    }
  }


  case class Incomes(value:Seq[Income],eventLog:Seq[IncomeEvent] = Seq()) extends (Incomes ==> Incomes) with produces[Seq[Income]]{
    override def apply(src: dataset[Incomes]): dataset[Incomes] = src.<--[Incomes]
    private def apply(income:Income):dataset[Incomes] = {
      val incomeMapCurr = this.incomeMap
      val exists = incomeMap.get(income.id).isDefined
      lazy val updatedMap = incomeMap.updated(income.id,income)
      val newIncomeColl = if(exists) updatedMap.values.toSeq else value :+ income
      new Incomes(newIncomeColl,this.eventLog){
        lazy val incomeMap = if (incomeMapCurr != null) updatedMap else Map(income.id -> income)
      }
    }
    private[Income] def addEvent(events:IncomeEvent*):dataset[Incomes] = new Incomes(this.value,events ++ this.eventLog)
    private lazy val incomeMap:Map[Long,Income] = value.map(a => a.id -> a).toMap

    def get(id:Long):Income = incomeMap(id)
    def update(acct:Income):dataset[Incomes] = apply(acct)
  }

  implicit class IncomeGrammar[A<:Incomes](src:dataset[A])(implicit taga:TypeTag[A]){
    def incomes:dataset[Incomes] = if(src.isInstanceOf[Incomes]) src else src.<--[Incomes]
    def events: produces[Seq[IncomeEvent]] = src.incomes
      .biMap[produces[Seq[IncomeEvent]]](err => noVal(err.value:_*))(
        d => someval(d.asInstanceOf[Incomes].eventLog)
      )
  }
  implicit class IncomeDateDrivenBehavior[A<:Incomes with Date with Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def generateIncomeEvents:dataset[IncomeEventGenerator] =
      (src +-IncomeEventGenerator(Seq())).<-+[IncomeEventGenerator]
    def receiveIncomes:dataset[A] = for{
      incomeEvents <- src.generateIncomeEvents
      incomes <- src.incomes
      accounts <- src.accounts
      updatedIncomes <- incomes.addEvent(incomeEvents.value:_*)
    }yield incomeEvents.foldLeft(src)((accumsrc,income) => for{
      acctPayable <- accounts.getAccount(income.payableTo).fromOption
    }yield accumsrc.deposit(acctPayable, income.amount)
    ) ++ updatedIncomes
  }


}

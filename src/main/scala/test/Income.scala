package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account._
import test.Date._

import scala.reflect.runtime.universe.TypeTag
object Income {
  type IncomeEventGenDeps = Incomes with Date
  type IncomeEventDeps = Date
  //define incomes
  trait Income
    extends (IncomeEventDeps ==> Income)
      with produces[Seq[IncomeEvent]]{
    val id:Long
    val payableTo:Long
    val amount:Double
    val dateRange:DateRange
  }
  case class ficaTaxableincome(id:Long,amount:Double,payableTo:Long, dateRange:DateRange,eventLog:Seq[IncomeEvent] = Seq()) extends Income{
    override val value = eventLog
    override def apply(src: dataset[IncomeEventDeps]): dataset[Income] =
    (for{
      date <- src.currentDate
      expandedDateRange <- dateRange.getRange
    }yield {
      if( this.isActive(date)) {
        val pr = expandedDateRange.findClosestPeriodRange(src)
        val res = pr.map(d => taxableIncomeEvent(this.id,this.payableTo,amount,d))
        this.copy(eventLog = res ++ eventLog,dateRange = expandedDateRange)
      }
      else this.copy(dateRange = expandedDateRange)
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
  case class taxableIncomeEvent(incomeId:Long,payableTo:Long,amount:Double,date:LocalDate) extends IncomeEvent


  case class Incomes(value:Seq[Income],eventLog:Seq[IncomeEvent] = Seq())
    extends (IncomeEventGenDeps ==> Incomes) with produces[Seq[Income]]{
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[Incomes] =  for{
      incomes <- src.incomes
    }yield {
      incomes.value.foldLeft(src)((accumSrc,i) => for{
        inc <- accumSrc.++(i).<-+[Income]
        incomes <- accumSrc.incomes
        updatedIncomes <- incomes.update(inc)
      }yield accumSrc ++ updatedIncomes).incomes
    }

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
        d => someval(d.asInstanceOf[Incomes].eventLog ++ d.asInstanceOf[Incomes].value.flatMap(_.value))
      )
    def eventsAtDate(date: Date):produces[Seq[IncomeEvent]] = someval(
      src.events.filter(e => date.isWithinPeriod(Year(e.date)))
    )
  }
  implicit class IncomeDateDrivenBehavior[A<:Incomes with Date with Accounts](src:dataset[A])(implicit taga:TypeTag[A]){
    def receiveIncomes:dataset[A] = for{
      date <- src.currentDate
      accounts <- src.accounts
      updatedIncomes <- src.<-+[Incomes]
    }yield (updatedIncomes.eventsAtDate(date).foldLeft(src ++ updatedIncomes)((accumsrc,income) => for{
      acctPayable <- accounts.getAccount(income.payableTo).fromOption
    }yield accumsrc.deposit(acctPayable, income.amount)))

  }


}

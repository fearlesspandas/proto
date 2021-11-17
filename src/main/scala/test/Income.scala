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


  //define income events
  trait IncomeEvent extends ::[IncomeEvent]{
    val incomeId:Long
    val payableTo:Long
    val amount:Double
    val date:LocalDate
  }
  case class taxableIncomeEvent(incomeId:Long,payableTo:Long,amount:Double,date:LocalDate) extends IncomeEvent

  //define incomes
  trait Income
    extends (IncomeEventDeps ==> Income)
      with produces[Seq[IncomeEvent]]{
    val id:Long
    val payableTo:Long
    val amount:Double
    val dateRange:DateRange
    def addEvents(events:IncomeEvent*):Income
  }
  case class TaxableIncome(id:Long, amount:Double, payableTo:Long, dateRange:DateRange, eventLog:Seq[IncomeEvent] = Seq()) extends Income{
    override val value = eventLog
    override def apply(src: dataset[IncomeEventDeps]): dataset[Income] =
    (for{
      date <- src.currentDate
    }yield {
      //this check is for performance not consistency
      //as if we're not in the active range we should
      //just see an empty range returned.
      if( this.isActive(date)) {
        val pr = dateRange.findClosestPeriodRange(src)
        val res = pr.map(d => taxableIncomeEvent(this.id,this.payableTo,amount,d))
        this.copy(eventLog = res ++ eventLog)
      }
      else this
    })
    def preceeds(date:Date):Boolean = date.isBefore(dateRange.start)
    def halted(date:Date):Boolean = date.isAfter(dateRange.end)
    def isActive(date:Date):Boolean = !preceeds(date) && !halted(date)

    override def addEvents(events: IncomeEvent*): Income = this.copy(eventLog = events ++ this.eventLog)
  }


  //define a collection of incomes
  case class Incomes(value:Seq[Income],eventLog:Seq[IncomeEvent] = Seq())
    extends (IncomeEventGenDeps ==> Incomes) with produces[Seq[Income]]{
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[Incomes] =  for{
      incomes <- src.incomes
    }yield {
      incomes.value.foldLeft(src)((accumSrc,i:Income) => for{
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
    private[Income] def addEvent(events:IncomeEvent*):dataset[Incomes] ={
      events.foldLeft(this)((accumincs,e) => {
        val income = accumincs.get(e.incomeId)
        accumincs.apply(income.addEvents(e)).get
      })
    }
    private lazy val incomeMap:Map[Long,Income] = value.map(a => a.id -> a).toMap

    def get(id:Long):Income = incomeMap(id)
    def update(acct:Income):dataset[Incomes] = apply(acct)
  }

  implicit class IncomeGrammar[A<:Incomes](src:dataset[A])(implicit taga:TypeTag[A]){
    def incomes:dataset[Incomes] = if(src.isInstanceOf[Incomes]) src else for{
      inc <-  src.<--[Incomes]
    } yield inc
    def events: produces[Seq[IncomeEvent]] = src.incomes
      .biMap[produces[Seq[IncomeEvent]]](err => noVal(err.value:_*))(
        d => someval(d.get.eventLog ++ d.get.value.flatMap(_.value))
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

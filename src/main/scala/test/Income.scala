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
    val startDate:Date
    val endDate:Date
  }
  case class ficaTaxableincome(id:Long,amount:Double,payableTo:Long, startDate:Date,endDate:Date,eventLog:Seq[IncomeEvent] = Seq()) extends Income{
    override val value = eventLog
    override def apply(src: dataset[IncomeEventGenDeps]): dataset[Income] =
    (for{
      date <- src.currentDate
    }yield date match {
      case _:Month|_:Week if((scala.math.abs(date.getDayOfMonth - startDate.getDayOfMonth) < date.numberOfDays))=>
        this.copy(eventLog = taxableIncomeEvent(this.id,this.payableTo,amount,date.value) +: eventLog)
    })
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
      date <- src.currentDate
    }yield {
        val toBePaidOut = incomes.value.flatMap(i => {
         src.++(i).<-+[Income].getValue[Seq[IncomeEvent]]
        })
        IncomeEventGenerator(
          toBePaidOut//.map(i => taxableIncomeEvent(i.id,i.amount,date))
        )
    }
  }


  case class Incomes(value:Seq[Income],eventLog:Seq[IncomeEvent] = Seq()) extends (Incomes ==> Incomes){
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
    def events:Val[Seq[IncomeEvent]] = for{
      incomes <- src.incomes
    }yield Val(incomes.eventLog)
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

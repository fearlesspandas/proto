package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account._
import test.Date._

import scala.reflect.runtime.universe.TypeTag
package object Income {
  type IncomeEventGenDeps = Incomes with Date
  type IncomeEventDeps = Date

  //define income events
  trait IncomeEvent extends ::[IncomeEvent] with Identifiable {
    val incomeId: Long
    val id = incomeId
    val payableTo: Long
    val amount: Double
    val date: LocalDate
  }

//define incomes
  trait Income extends ModelEventLog[IncomeEvent, IncomeEventDeps, Income] {
    val id: Long
    val payableTo: Long
    val amount: Double
    val dateRange: DateRange

    def isActive(date: Date): Boolean = !preceeds(date) && !halted(date)

    def preceeds(date: Date): Boolean = date.isBefore(dateRange.start)

    def halted(date: Date): Boolean = date.isAfter(dateRange.end)
  }
  case class taxableIncomeEvent(incomeId: Long, payableTo: Long, amount: Double, date: LocalDate)
      extends IncomeEvent

  case class TaxableIncome(
    id: Long,
    amount: Double,
    payableTo: Long,
    dateRange: DateRange,
    eventLog: Seq[IncomeEvent] = Seq()
  ) extends Income {
    override val value = eventLog
    def apply(src: dataset[IncomeEventDeps]): dataset[Income] =
      (for {
        date <- src.currentDate
      } yield {
        //this check is for performance not consistency
        //as if we're not in the active range we should
        //just see an empty range returned.
        if (this.isActive(date)) {
          val pr = dateRange.findClosestPeriodRange(src)
          val res = pr.map(d => taxableIncomeEvent(this.id, this.payableTo, amount, d))
          addEvent(res: _*)
        } else this
      })
    def addEvent(events: IncomeEvent*): dataset[Income] =
      this.copy(eventLog = events ++ this.eventLog)
  }

  //define a collection of incomes
  case class Incomes(value: Seq[Income], eventLog: Seq[IncomeEvent] = Seq())(
    implicit val taga: TypeTag[Income],
    val tagself: TypeTag[Income.Incomes],
    val tagdeps: TypeTag[IncomeEventGenDeps]
  ) extends EventBasedModelContainer[IncomeEvent, IncomeEventGenDeps, Income, Incomes] {
    override def apply(coll: Map[Long, Income]): dataset[Incomes] = new Incomes(coll.values.toSeq)
  }

  implicit class IncomeGrammar[A <: Incomes](src: dataset[A])(implicit taga: TypeTag[A]) {
    def eventsAtDate(date: Date): Seq[IncomeEvent] =
      src.events.filter(e => date.isWithinPeriod(Year(e.date)))

    def events: Seq[IncomeEvent] =
      src.incomes
        .biMap[Seq[IncomeEvent]](err => Seq())(d => d.get.eventLog ++ d.get.value.flatMap(_.value))

    def incomes: dataset[Incomes] =
      if (src.isInstanceOf[Incomes]) src
      else
        for {
          inc <- src.<--[Incomes]
        } yield inc
  }
  implicit class IncomeDateDrivenBehavior[A <: Incomes with Date with Accounts](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {

    def receiveIncomes: dataset[A] =
      for {
        date <- src.currentDate
        accounts <- src.accounts
        updatedIncomes <- src.<-+[Incomes]
      } yield (
        updatedIncomes
          .eventsAtDate(date)
          .foldLeft(src ++ updatedIncomes)((accumsrc, income) =>
            for {
              acctPayable <- accounts.getAccount(income.payableTo).fromOption
            } yield accumsrc.deposit(acctPayable, income.amount)
          )
        )

  }

}

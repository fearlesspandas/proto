package test

import java.time.LocalDate

import Typical.core.grammar._
import Typical.core.dataset.{produces, _}
import test.Date.DateRange
import test.Expense.{ExpManDeps}
import test.Property._

import scala.reflect.runtime.universe._
object Expense {
trait Expense{
  val id:Long
  val amount:Double
  val date:LocalDate
}
  type ExpManDeps = Properties
  case class Expenses(value:Seq[Expense]) extends (ExpManDeps ==> Expenses) with produces[Seq[Expense]]{
    override def apply(src: dataset[ExpManDeps]): dataset[Expenses] = for{
      properties <- src.properties
    }yield Expenses(properties.events.collect({case e: Expense => e}))
  }
  implicit class ExpManGrammar[A<:ExpManDeps](src:dataset[A])(implicit taga:TypeTag[A]){
    def expenses:dataset[Expenses] = src.<-+[Expenses]
    def expensesAtDate(date:LocalDate):dataset[Expenses] = for{
      exp <- src.expenses
    }yield Expenses(exp.collect({case e:Expense if e.date == date => e}))
  }
}

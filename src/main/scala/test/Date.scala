package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import scala.reflect.runtime.universe.TypeTag
object Date{
  implicit class DateGrammar[A<:Date](src:dataset[A])(implicit taga:TypeTag[A]) {
    def currentDate:dataset[Date] = if(src.isInstanceOf[Date]) src else src.<--[Date]
    def toYearlyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.++[Date,Year](Year(date))
    def toMonthlyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.++[Date,Month](Month(date))
    def toWeeklyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.++[Date,Week](Week(date))
    def toDailyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.++[Date,Day](Day(date))
    def nextPeriod:dataset[A] = src.+->[Date]
  }

  sealed trait Date extends index[Date] with produces[LocalDate]{
    val periodsInYear:Int
    val numberOfDays:Int
  }

  case class Day(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Day(value.plusDays(1))
    override val periodsInYear: Int = 365
    override val numberOfDays: Int = 1
  }
  case class Week(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Week(value.plusWeeks(1))
    override val periodsInYear: Int = 52
    override val numberOfDays: Int = 7
  }
  case class Month(value :LocalDate) extends Date{
    override def apply(): dataset[Date] = Month(value.plusMonths(1))
    override val periodsInYear: Int = 12
    override val numberOfDays: Int = 30
  }
  case class Year(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Year(value.plusYears(1))
    override val periodsInYear: Int = 1
    override val numberOfDays: Int = 365
  }


}

package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import scala.reflect.runtime.universe.TypeTag
object Date{
  implicit class DateGrammar[A<:Date](src:dataset[A])(implicit taga:TypeTag[A]) {
    def currentDate:dataset[Date] = if(src.isInstanceOf[Date]) src else src.fetch[Date]
    def toYearlyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.include[Date,Year](Year(date))
    def toMonthlyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.include[Date,Month](Month(date))
    def toDailyCadence:dataset[A] = for{
      date <- src.currentDate
    }yield src.include[Date,Day](Day(date))
    def nextPeriod:dataset[A] = src.iter[Date]
  }

  sealed trait Date extends index[Date] with produces [LocalDate]{
    val periodsInYear:Int
  }

  case class Day(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Day(value.plusDays(1))

    override val periodsInYear: Int = 365

  }

  case class Month(value :LocalDate) extends Date{
    override def apply(): dataset[Date] = Month(value.plusMonths(1))

    override val periodsInYear: Int = 12
  }
  case class Year(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Year(value.plusYears(1))

    override val periodsInYear: Int = 1
  }


}

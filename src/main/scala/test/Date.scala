package test

import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._

import scala.reflect.runtime.universe.TypeTag
package object Date {
  implicit class DateGrammar[A <: Date](src: dataset[A])(implicit taga: TypeTag[A]) {

    def toYearlyCadence: dataset[A] =
      for {
        date <- src.currentDate
      } yield src.++[Date, Year](Year(date))

    def currentDate: dataset[Date] = if (src.isInstanceOf[Date]) src else src.<--[Date]

    def toMonthlyCadence: dataset[A] =
      for {
        date <- src.currentDate
      } yield src.++[Date, Month](Month(date))

    def toWeeklyCadence: dataset[A] =
      for {
        date <- src.currentDate
      } yield src.++[Date, Week](Week(date))

    def toDailyCadence: dataset[A] =
      for {
        date <- src.currentDate
      } yield src.++[Date, Day](Day(date))

    def nextPeriod: dataset[A] = src.+->[Date]
  }
  //index is a model of type self ===> self
  sealed trait Date extends index[Date] with produces[LocalDate] {
    val periodsInYear: Int
    val numberOfDays: Int
  }

 sealed trait DateRange extends (Date ==> DateRange) with produces[Seq[Date]] {
    val start: Date
    val end: LocalDate
    def apply(newvalue: Seq[Date]): DateRange

    override def apply(src: dataset[Date]): dataset[DateRange] =
      for {
        date <- src.currentDate
      } yield date match {
        case d if (d.isBefore(end)) =>
          for {
            nextSeq <- (src).nextPeriod
              .<-+[DateRange] //we could alternatively recursively call apply(src.nextPeriod) here as that is what <-+[DateRange] reduces to in this case; in theory however this might be 'slightly' better given that it still allows us to completely abstract away the definition of apply for date range, giving us potential for heirarrchies
          } yield apply(
            date +: nextSeq
          )
        case _ => apply(Seq.empty[Date])
      }

    def findClosestPeriod(src: dataset[Date]): dataset[Date] =
      for {
        date <- src.currentDate
      } yield {
        val filteresres = this
          .filter(d => date.isWithinPeriod(d) && d.isWithinPeriod(date))
        filteresres
          .sortWith((a, b) =>
            scala.math.abs(a.getDayOfYear - date.getDayOfYear) < scala.math.abs(b.getDayOfYear - date.getDayOfYear)
          )
          .headOption
          .fromOption
      }
    def findClosestPeriodRange(src: dataset[Date]): produces[Seq[Date]] =
      (for {
        date <- src.currentDate
      } yield {
        val withinPeriod = this
          .filter(d => date.isWithinPeriod(d))
        apply(withinPeriod)
      }).getOrElse(Seq.empty[Date])
  }

 //each subtype defines how their respective index would be updated
  case class Day(value: LocalDate) extends Date {
    override val periodsInYear: Int = if (!value.isLeapYear) 365 else 366
    override val numberOfDays: Int = 1

   override def apply(): dataset[Date] = Day(value.plusDays(1))
  }

 case class Week(value: LocalDate) extends Date {
    override val periodsInYear: Int = 52
    override val numberOfDays: Int = 7

   override def apply(): dataset[Date] = Week(value.plusWeeks(1))
  }

 case class Month(value: LocalDate) extends Date {
    override val periodsInYear: Int = 12
    override val numberOfDays: Int = monthToDay(this)

   override def apply(): dataset[Date] = Month(value.plusMonths(1))
  }

  case class Year(value: LocalDate) extends Date {
    override val periodsInYear: Int = 1
    override val numberOfDays: Int = if (!value.isLeapYear) 365 else 366

   override def apply(): dataset[Date] = Year(value.plusYears(1))
  }

  implicit class DateRangeGrammar[A <: DateRange](src: dataset[A])(implicit taga: TypeTag[A]) {
    private[Date] def getRange: dataset[DateRange] =
      if (src.dateRange.getValue.exists && src.dateRange.getValue.value.nonEmpty)
        src
      else
        for {
          range <- src.dateRange
          sd <- range.start
          expandedDateRange <- (
            data[Date]() ++
              sd ++
              range
          ).<-+[DateRange]
        } yield expandedDateRange

    def dateRange: dataset[DateRange] = if (src.isInstanceOf[DateRange]) src else src.<--[DateRange]
  }

  class dates(val start: Date, val end: LocalDate, val value: Seq[Date] = Seq()) extends DateRange {
    override def apply(newvalue: Seq[Date]): DateRange = new dates(start, end, newvalue)
  }
  object dates {
    def apply(start: Date, end: LocalDate): DateRange = new dates(start, end).getRange.get
  }
  object monthToDay {
    def apply(date: Date): Int = date.getMonthValue match {
      case 1  => 31
      case 2  => if (!date.isLeapYear) 28 else 29
      case 3  => 31
      case 4  => 30
      case 5  => 31
      case 6  => 30
      case 7  => 31
      case 8  => 31
      case 9  => 30
      case 10 => 31
      case 11 => 30
      case 12 => 31
    }
  }
  implicit class TimeHandler(src: Date) {
    //src is our context which includes a current date with a frequency/cadence
    //date is a date with bounds (defined by the cadence)
    // here we test if src is within the bounds of date with respect to their relative cadences
    def isWithinPeriod(date: Date): Boolean = src match {
      //if src/context is daily, recursively check src year is within date year, and check if they're on the same day exactly
      case _: Day => Year(src.value).isWithinPeriod(date) && date.getDayOfYear == src.getDayOfYear
      //if src/context is weekly, recursively check src and date are within the same month, then check if there within a week of eachother
      case _: Week =>
        Month(src.value)
          .isWithinPeriod(date) && (src.getDayOfYear - date.getDayOfYear) < 7 && (src.getDayOfYear - date.getDayOfYear >= 0)
      //if src/context is monthly, recursively check if they're within the same year, then check if they're in the same month
      case _: Month => Year(src.value).isWithinPeriod(date) && src.getMonth == date.getMonth
      //if src/context is yearly, check if years match
      case _: Year => src.getYear == date.getYear
    }

    /*
    example 1
    src:dataset[Day](Day(01-01-2021)) i.e. src iterates on a daily cadence
    date:Year(03-07-2021) i.e. a yearly event on 03-07-2021

    src.currentDate.isWithinPeriod(date) -> false

    but if we iterate src.nextPeriod until src.currentDate = Day(03-07-2021)
    then src.currentDate.isWithinPeriod(date) -> true

    example 2
    src:dataset[Year](Year(01-01-2021) i.e. iterates on a yearly cadence
    date:Day(03-07-2021) i.e. a daily event on 03-07-2021

    in this case we have
    src.currentDate.isWithinPeriod(date) -> true
    because relative to src's frequency, date will occur before src's next iteration

   */
  }

}

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
    override val periodsInYear: Int = if(!value.isLeapYear)365 else 366
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
    override val numberOfDays: Int =  monthToDay(this)
  }
  case class Year(value:LocalDate) extends Date{
    override def apply(): dataset[Date] = Year(value.plusYears(1))
    override val periodsInYear: Int = 1
    override val numberOfDays: Int = if(!value.isLeapYear)365 else 366
  }

  import scala.math.abs

  sealed trait DateRange extends (Date ==> DateRange) with produces[Seq[Date]]{
    val start:Date
    val end:LocalDate
    def apply(newvalue:Seq[Date]):DateRange

    override def apply(src: dataset[Date]): dataset[DateRange] = for{
      date <- src.currentDate
    }yield  date match{
      case d if (d.isBefore(end)) => for{
        nextSeq <- (src).nextPeriod.<-+[DateRange]
      }yield
        apply(
          date +: nextSeq
        )
      case _ => apply(Seq.empty[Date])
    }


    def findClosestPeriod(src:dataset[Date]):dataset[Date] = for{
      date <- src.currentDate
    }yield {
          val filteresres = this
            .filter(d => date.isWithinPeriod(d) && d.isWithinPeriod(date))
            filteresres
              .sortWith(
                (a,b) => abs(a.getDayOfYear - date.getDayOfYear) < abs(b.getDayOfYear - date.getDayOfYear)
              )
              .headOption.fromOption
    }
    def findClosestPeriodRange(src:dataset[Date]):produces[Seq[Date]] = (for{
      date <- src.currentDate
    }yield {
      val withinPeriod = this
        .filter(d => date.isWithinPeriod(d))
        apply(withinPeriod)
    }).getOrElse(Seq.empty[Date])
  }

  implicit class DateRangeGrammar[A<:DateRange](src:dataset[A])(implicit taga:TypeTag[A]){
    def dateRange:dataset[DateRange] = if(src.isInstanceOf[DateRange]) src else src.<--[DateRange]
    def getRange:dataset[DateRange] =if(src.dateRange.getValue.exists && src.dateRange.getValue.value.size > 0)
      src
    else for {
      range <- src.dateRange
      sd <- range.start
      expandedDateRange <- (
        data[Date]() ++
          sd ++
          range
        )
        .<-+[DateRange]
    }yield expandedDateRange
  }


  case class dates(start:Date,end:LocalDate,value:Seq[Date] = Seq()) extends DateRange {
    override def apply(newvalue: Seq[Date]): DateRange = dates(start,end,newvalue)
  }

  object monthToDay {
    def apply(date:Date):Int = date.getMonthValue match {
      case 1 =>31
      case 2 =>if(!date.isLeapYear) 28 else 29
      case 3 => 31
      case 4 => 30
      case 5 => 31
      case 6 => 30
      case 7 => 31
      case 8 => 31
      case 9 => 30
      case 10 => 31
      case 11 => 30
      case 12 => 31
    }
  }
  implicit class TimeHandler(src:Date){

    def isWithinPeriod(date:Date):Boolean = src match {
      case _:Day => Year(src.value).isWithinPeriod(date) && date.getDayOfYear == src.getDayOfYear
      case _:Week => Month(src.value).isWithinPeriod(date) && (src.getDayOfYear - date.getDayOfYear) < 7 && (src.getDayOfYear - date.getDayOfYear >= 0)
      case _:Month => Year(src.value).isWithinPeriod(date) && src.getMonth == date.getMonth
      case _:Year =>  src.getYear == date.getYear
    }
  }

}

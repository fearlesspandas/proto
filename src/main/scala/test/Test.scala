package test

import java.time.LocalDate

import test.SpendWildly.SpendWildly

import scala.annotation.implicitNotFound


object runner {
  import Typical.core._
  import Account._
  import GrowAccounts._
  import grammar._
  import dataset._
  import Property._
  import Date._
  println("Initializing")
  val starterAccounts = Accounts(Seq(CheckingAccount(1,100),BokerageAccount(2,10000)),Seq())
  val starterProperties = Properties(Seq(RentalProperty(1,1300)),Seq())
  val startingDate = Month(LocalDate.of(2021,1,1))
  type ProgramDependencies =
      Accounts     with
      AccountRates with
      Properties   with
      Date

  val dat = data[Date]()
    //define start data
    .include[Date,Month](startingDate)
    .include(starterAccounts)
    .include(AccountRates())
    .include(starterProperties)
    //include the program we want to loop over
    // if we dont' want to build an implicit grammar for it
    .include(Prog())

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    //run base loop
    dat.run[Prog].console()
    val end = System.currentTimeMillis()
    println(s"time elapsed:${end - start} milliseconds")
  }
  case class Prog() extends ==>[ProgramDependencies, ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = for{
      date <- src.currentDate
    }yield{
      date match {
        case m:Month =>
          src
          .growAccounts
          .accrueRent
          .payRents
          .nextPeriod
        case y:Year =>
          (0 until 12).foldLeft(src)((src_,_) =>
            src_
              .toMonthlyCadence
              .growAccounts
              .accrueRent
              .payRents
              .nextPeriod
          ).toYearlyCadence
      }

    }
  }


}

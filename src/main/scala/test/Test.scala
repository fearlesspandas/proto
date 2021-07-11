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
  import AccountRates._
  import Income._
  println("Initializing")
  val starterAccounts = Accounts(Seq(CheckingAccount(1,30000),BokerageAccount(2,60000)),Seq())
  val rentPeriod = dates(Month(LocalDate.now()),LocalDate.now().plusYears(20))
  val starterProperties = Properties(Seq(RentalProperty(1,1300,rentPeriod)),Seq())
  val startingDate = Month(LocalDate.now())
  val incomemeta = Seq(
    ficaTaxableincome(1,3500,1,dates(Month(LocalDate.now()),LocalDate.now().plusYears(10),Seq())),
    ficaTaxableincome(2,500,1,dates(Week(LocalDate.now().plusMonths(6)),LocalDate.now().plusYears(2),Seq())),
    ficaTaxableincome(3,5500,1,dates(Year(LocalDate.now()),LocalDate.now().plusYears(10),Seq())),
    ficaTaxableincome(4,1500,1,dates(Month(LocalDate.now().plusYears(1)),LocalDate.now().plusYears(10),Seq()))
  )
  val incomes = Incomes(incomemeta)
  type ProgramDependencies =
      Accounts     with
      AccountRates with
      Properties   with
      Date         with
      Incomes

  val dat:dataset[ProgramDependencies] = data[ProgramDependencies]()
    //define start data
    .++[Date,Month](startingDate)++
    starterAccounts              ++
    AccountRates(0.67,0.01,0.06) ++
    incomes                      ++
    starterProperties            +-
    Prog(1000000)
    //include the program we want to loop over
    // if we dont' want to build an implicit grammar for it

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    //run base loop
    val res = dat.toWeeklyCadence.solve[Prog].properties.events.sortWith((a,b) => a.date.isBefore(b.date))//.get.get(3).value//.events.filter(_.incomeId == 1)
    println(res)
    val end = System.currentTimeMillis()
    println(s"time elapsed:${end - start} milliseconds")
  }
  case class Prog(limit:Double) extends (ProgramDependencies ==> ProgramDependencies) with solveable[ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = for{
      date <- src.currentDate
    }yield{
      date match {
        case _:Month| _:Week =>
          src
          .growAccounts
          .accrueRent
          .receiveIncomes
          .payRents
          .nextPeriod
        case y:Year =>
          (0 until 12).foldLeft(src)((src_,_) =>
            src_
              .toMonthlyCadence
            .runSim
          ).toYearlyCadence
      }
    }

    override def solved(src: dataset[ProgramDependencies]):Boolean =
      (src
          .underlyingAccounts.exists) &&
            src.underlyingAccounts
          .value
          .map(_.balance)
          .sum > limit

    override def next(src: dataset[ProgramDependencies]): Seq[dataset[_]] = Seq(
      src.runSim
    )
  }
  import scala.reflect.runtime.universe.TypeTag
  implicit class RunSim[A<:ProgramDependencies](src:dataset[A])(implicit taga:TypeTag[A]){
    def runSim:dataset[A] = src.+-(Prog(10000000)).-->[Prog]
  }


}

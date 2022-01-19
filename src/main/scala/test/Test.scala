package test

import java.time.LocalDate

import test.Fields.Bal

import scala.annotation.implicitNotFound

package object Program{

  import Typical.core._
  import Account._
  import AccountUtils._
  import grammar._
  import dataset._
  import Property._
  import Date._
  import AccountRates._
  import Income._
  import test.Fields._

  type ProgramDependencies =
    Accounts     with
      AccountRates with
      Properties   with
      Date         with
      Incomes
  ////////////////////////////////////
  //define our top level simulation
  case class Prog(limit:Double)
    extends (ProgramDependencies ==> ProgramDependencies)
      with solveable[ProgramDependencies] {
    override def apply(src: dataset[ProgramDependencies]): dataset[ProgramDependencies] = for{
      date <- src.currentDate
    }yield{
      date match {
        case _:Month| _:Week | _:Day =>
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
          .map(_.bal.get.value.getOrElse(0d))
          .sum > limit

    override def next(src: dataset[ProgramDependencies]): Seq[dataset[ProgramDependencies]] = Seq(
      src.-->[Prog]
    )
  }
  ////////////////////////////////
  //define implicit grammar for sim
  import scala.reflect.runtime.universe.TypeTag
  implicit class RunSim[A<:ProgramDependencies](src:dataset[A])(implicit taga:TypeTag[A]){
    def runSim:dataset[A] = src.+-(Prog(10000)).-->[Prog]
  }

}
object runner {
  import Program._
  import Typical.core._
  import Account._
  import grammar._
  import dataset._
  import Property._
  import Date._
  import AccountRates._
  import Income._

  println("Initializing")



  object Testy extends Enumeration{
    val Thing = Value
  }

  /////////////////////////////////////////////////////////////
  ///////////Define Starting Data/////////////////////////////
  val starterAccounts = Accounts(Seq(CheckingAccount(1,30000),BokerageAccount(2,60000)))

  val rentPeriod = dates(Month(LocalDate.now()),LocalDate.now().plusYears(20))

  val starterProperties = Properties(Seq(RentalProperty(1,1300,rentPeriod)))

  val startingDate = Month(LocalDate.now())

  val incomemeta = Seq(
    TaxableIncome(1,3500,1,dates(Month(LocalDate.now()),LocalDate.now().plusYears(10))),
    TaxableIncome(2,500,1,dates(Week(LocalDate.now().plusMonths(6)),LocalDate.now().plusYears(2))),
    TaxableIncome(3,5500,1,dates(Year(LocalDate.now()),LocalDate.now().plusYears(10))),
    TaxableIncome(4,1500,1,dates(Month(LocalDate.now().plusYears(1)),LocalDate.now().plusYears(10)))
  )
  val incomes = Incomes(incomemeta)

  val dat:dataset[ProgramDependencies] = data[ProgramDependencies]().asInstanceOf[dataset[ProgramDependencies]]
    //include initial data
    .++[Date,Month](startingDate)++
    starterAccounts              ++
    AccountRates(0.67,0.01,0.06) ++
    incomes                      ++
    starterProperties            +-
    //include the program we want to loop over
    // if we dont' want to build an implicit grammar for it
    Prog(1000000)

  trait thinggg[+A]
  class thingy extends thinggg[Int with String]
  //(new thingy).asInstanceOf[Int with Double]
  //trait Entity

  trait Person
  trait Trust
  trait Other
  trait Household
  trait Joint
  trait Owner

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    //run solve baseLoop for solve condition
    val res:data[ProgramDependencies] =
      dat
      .toWeeklyCadence
        .solve[Prog]
        .asInstanceOf[data[ProgramDependencies]]
    println(res.context)
    val end = System.currentTimeMillis()
    println(s"time elapsed:${end - start} milliseconds")
    //res.console
    import test.Fields._
    val ctx = (res.getAccount(1).fromOption)
    val acct = res.getAccount(1).fromOption.get
    println (( ctx ).bal.getValue.get)
    println(acct.balance)
  }


}

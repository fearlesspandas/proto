package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Account._
import EventHandler._
import Date._
import scala.reflect.runtime.universe.TypeTag


object Property{
  //define property types

  trait Property extends ::[Property]{
    val id:Long
  }
  case class RentalProperty(id:Long, monthlyRent:Double) extends Property
  case class Home(id:Long,costBasis:Double,marketValue:Double)
  //define property events
  case class rentPaymentDue(propertyId:Long,amount:Double,date:LocalDate) extends Event
  case class rentPaymentPaid(propertyId:Long,amount:Double,date:LocalDate) extends Event

//  case class GenerateRentPayments(value:Seq[rentPaymentPaid]) extends
//    (Properties with Date ==> GenerateRentPayments) with
//  produces[Seq[rentPaymentPaid]]{
//    override def apply(src: dataset[Properties with Date]): dataset[GenerateRentPayments] = for{
//      properties <- src.properties
//      date <- src.currentDate
//    }yield properties.events.collect({case r:rentPaymentDue => rentPaymentPaid(r.propertyId,r.amount,date)})
//  }
  case class GenerateRentDue(value:Seq[rentPaymentDue]) extends
    (Properties with Date ==> GenerateRentDue) with
    produces[Seq[rentPaymentDue]]{
    override def apply(src: dataset[Properties with Date]): dataset[GenerateRentDue] = for{
      properties <- src.properties
      date <- src.currentDate
    }yield GenerateRentDue(
         date match {
          case m:Month => properties.collect({
            case r:RentalProperty => rentPaymentDue(r.id,r.monthlyRent,m)
          })
          case y:Year => properties.collect({
            case r:RentalProperty => (0 to 11).foldLeft(Seq[rentPaymentDue]())(
              (accum,n) => rentPaymentDue(r.id,r.monthlyRent,y.value.plusMonths(n)) +: accum
            )
          }).flatten
        })

  }
  implicit class RentGeneratorGrammarPaymentHelper[A<:Properties with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def monthlyRent:dataset[GenerateRentDue] =
      src
      .includeIfNotPresent(GenerateRentDue(Seq()))
      .derive[GenerateRentDue]
  }


  case class Properties(value:Seq[Property],eventLog:Seq[Event]) extends (Properties ==> Properties) with produces[Seq[Property]]{
    lazy val propertyMap: Map[Long,Property] = value.map(a => a.id -> a).toMap
    override def apply(v1: dataset[Properties]): dataset[Properties] = v1.fetch[Properties]
    private def apply(account:Property):dataset[Properties] = {
      val pptyMap = this.propertyMap
      val exists = pptyMap.get(account.id).isDefined
      lazy val updatedMap = pptyMap.updated(account.id,account)
      val newAcctColl = if(exists) updatedMap.values.toSeq else value :+ account
      new Properties(newAcctColl,this.eventLog){
        override lazy val propertyMap = if (pptyMap != null) updatedMap else Map(account.id -> account)
      }
    }
    private[Property] def addEvents(events:Event*) = Properties(value,events ++ eventLog)
    def get(id:Long):dataset[Property] = propertyMap(id)
    def update(property:Property):dataset[Properties] = apply(property)
    //implicit class PropertiesAPI[](src:)

  }
  implicit class PropertiesAPI[A<:Properties](src:dataset[A])(implicit taga:TypeTag[A]){
    def properties:dataset[Properties] = if(src.isInstanceOf[Properties]) src else src.fetch[Properties]
    def events:Val[Seq[Event]] = for{
      properties <- src.properties
    }yield Val(properties.eventLog)
  }
  implicit class GrowRents[A<:Properties with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def accrueRent:dataset[A] = for{
      properties <- src.properties
      monthlyRentPayments <- src.monthlyRent
    }yield src.include(
      properties.addEvents(monthlyRentPayments.value:_*)
    )
  }
  implicit class PayRents[A<:Properties with Accounts with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def payRents:dataset[A] = for{
      properties <- src.properties
      accounts <- src.accounts
      date <- src.currentDate
      rentAcct <- accounts
        .collectFirst({case c:CheckingAccount => c})
        .asInstanceOf[Option[Account]]
        .fromOption
    }yield date match {
      case m:Month =>
        val rentpaymentsdue = properties.events.collect({
          case r:rentPaymentDue if r.date == date.value => r
        })
        rentpaymentsdue.foldLeft(src)((accumSrc,paymentDue) =>
          accumSrc
          .spend(rentAcct,paymentDue.amount)
            .include(
              properties.addEvents(
                rentPaymentPaid(paymentDue.propertyId,paymentDue.amount,date)
              )
            )
        )
      case _ => DatasetError[A](new Error("rent payments other than Monthly not implemented"))


    }
  }

}

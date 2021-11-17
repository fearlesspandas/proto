package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import Account._
import EventHandler._
import Date._
import scala.reflect.runtime.universe.TypeTag


package object Property{
  //define property types
  trait propertyEvent{
    val propertyId:Long
    val amount:Double
    val date:LocalDate
  }
  type PropertyEventDeps = Date
  trait Property extends (PropertyEventDeps ==> Property) with produces[Seq[propertyEvent]]{
    val id:Long
    val dateRange:DateRange
    def addEvents(events:propertyEvent*):Property
    def preceeds(date:Date):Boolean = date.isBefore(dateRange.start)
    def halted(date:Date):Boolean = date.isAfter(dateRange.end)
    def isActive(date:Date):Boolean = !preceeds(date) && !halted(date)
  }
  case class RentalProperty(id:Long, rent:Double,dateRange:DateRange,eventLog:Seq[propertyEvent] = Seq()) extends Property{
    override val value = eventLog
    override def apply(src: dataset[PropertyEventDeps]): dataset[Property] = for{
      date <- src.currentDate
    }yield {
      if(isActive(date)){
        val events = dateRange.findClosestPeriodRange(src).map(d => rentPaymentDue(id,rent,d)) ++ eventLog
        this.copy(eventLog = events)
      }else this
    }

    override def addEvents(events: propertyEvent*): Property = this.copy(eventLog = events ++ this.eventLog)
  }
  case class Home(id:Long,costBasis:Double,marketValue:Double,dateRange:DateRange,value:Seq[propertyEvent] = Seq()) extends Property{
    override def apply(v1: dataset[PropertyEventDeps]): dataset[Property] = this

    override def addEvents(events: propertyEvent*): Property = this.copy(value = events ++ this.value)
  }
  //define property events
  case class rentPaymentDue(propertyId:Long,amount:Double,date:LocalDate) extends propertyEvent
  case class rentPaymentPaid(propertyId:Long,amount:Double,date:LocalDate) extends propertyEvent
  //generate rend due payments in model


  type PropertyEventGenDeps =  Date
  case class Properties(value:Seq[Property],eventLog:Seq[propertyEvent]) extends (PropertyEventGenDeps with Properties ==> Properties) with produces[Seq[Property]]{
    lazy val propertyMap: Map[Long,Property] = value.map(a => a.id -> a).toMap
    override def apply(src: dataset[PropertyEventGenDeps with Properties]): dataset[Properties] = for{
      properties <- src.properties
    }yield {
      properties.value.foldLeft(src)((accumSrc, p) => for {
        prop <- accumSrc.++(p).<-+[Property]
        accumProps <- accumSrc.properties
        updatedProps <- accumProps.update(prop)
      } yield accumSrc ++ updatedProps).properties
    }

    private def apply(property:Property):dataset[Properties] = {
      val pptyMap = this.propertyMap
      val exists = pptyMap.get(property.id).isDefined
      lazy val updatedMap = pptyMap.updated(property.id,property)
      val newAcctColl = if(exists) updatedMap.values.toSeq else value :+ property
      new Properties(newAcctColl,this.eventLog){
        override lazy val propertyMap = if (pptyMap != null) updatedMap else Map(property.id -> property)
      }
    }
    private[Property] def addEvents(events:propertyEvent*) = {
      events.foldLeft(this)((accumprops,e) => {
        val prop = accumprops.get(e.propertyId)
        val updatedProp = prop.get.addEvents(e)
        accumprops.apply(updatedProp).get
      })
      //this.copy(eventLog = events ++ eventLog)
    }
    def get(id:Long):dataset[Property] = propertyMap(id)
    def update(property:Property):dataset[Properties] = apply(property)

  }
  implicit class PropertiesAPI[A<:Properties](src:dataset[A])(implicit taga:TypeTag[A]){
    def properties:dataset[Properties] = if(src.isInstanceOf[Properties]) src else src.<--[Properties]
    def events:produces[Seq[propertyEvent]] = src.properties.biMap[produces[Seq[propertyEvent]]](err => noVal(err.value:_*))(d => someval(d.get.eventLog ++ d.get.value.flatMap(_.value)))
    def eventsAtDate(date: Date):produces[Seq[propertyEvent]] = someval(
      src.events.filter(e => date.isWithinPeriod(Year(e.date)))
    )
  }
  implicit class GrowRents[A<:Properties with Date](src:dataset[A])(implicit taga:TypeTag[A]){
    def accrueRent:dataset[A] = src.+->[Properties]
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
    }yield {
      val rentPaymentsPaid = properties.eventsAtDate(date).collect({
        case r:rentPaymentDue => rentPaymentPaid(r.propertyId,r.amount,r.date)
      })
      rentPaymentsPaid.foldLeft(src)(
        (accumSrc,event) =>
          accumSrc.withdraw(rentAcct,event.amount)
      ) ++ properties.addEvents(rentPaymentsPaid:_*)
    }
  }

}

package test
import java.time.LocalDate

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account._
import test.Date._
import test.Expense.Expense

import scala.reflect.runtime.universe.TypeTag

package object Property {

type PropertyEventDeps = Date
  type PropertyEventGenDeps = Date

//define property types
  trait propertyEvent extends Identifiable {
    val propertyId: Long
    val id = propertyId
    val amount: Double
    val date: LocalDate
  }

trait Property
      extends (PropertyEventDeps ==> Property)
      with EventBased[propertyEvent, Property]
      with Identifiable
      with produces[Seq[propertyEvent]] {
    val id: Long
    val dateRange: DateRange
    val eventLog: Seq[propertyEvent]
    val value = eventLog

  def isActive(date: Date): Boolean = !preceeds(date) && !halted(date)

  def preceeds(date: Date): Boolean = date.isBefore(dateRange.start)

  def halted(date: Date): Boolean = date.isAfter(dateRange.end)
  }

case class RentalProperty(
    id: Long,
    rent: Double,
    dateRange: DateRange,
    eventLog: Seq[propertyEvent] = Seq()
  ) extends Property {
    override def apply(src: dataset[PropertyEventDeps]): dataset[Property] =
      for {
        date <- src.currentDate
      } yield {
        if (isActive(date)) {
          val events = dateRange.findClosestPeriodRange(src).map(d => rentPaymentDue(id, rent, d))
          this.addEvent(events: _*)
        } else this
      }

    def addEvent(events: propertyEvent*): dataset[Property] =
      this.copy(eventLog = events ++ this.eventLog)
  }

case class Home(
    id: Long,
    costBasis: Double,
    marketValue: Double,
    dateRange: DateRange,
    eventLog: Seq[propertyEvent] = Seq()
  ) extends Property {
    override def apply(v1: dataset[PropertyEventDeps]): dataset[Property] = this

    def addEvent(events: propertyEvent*): dataset[Property] =
      this.copy(eventLog = events ++ this.eventLog)
  }

//define property events
  case class rentPaymentDue(propertyId: Long, amount: Double, date: LocalDate)
      extends propertyEvent
      with Expense
  //generate rend due payments in model

  case class rentPaymentPaid(propertyId: Long, amount: Double, date: LocalDate)
      extends propertyEvent

case class Properties(value: Seq[Property])(
    implicit val tagself: TypeTag[Properties],
    val taga: TypeTag[Property],
    val tagdeps: TypeTag[PropertyEventDeps with Properties]
  ) extends EventBasedModelContainer[
        propertyEvent,
        PropertyEventDeps with Properties,
        Property,
        Properties
      ] {
    override def apply(coll: Map[Long, Property]): dataset[Properties] =
      Properties(coll.values.toSeq)

  }
  implicit class PropertiesAPI[A <: Properties](src: dataset[A])(implicit taga: TypeTag[A]) {
    def eventsAtDate(date: Date): produces[Seq[propertyEvent]] = someval(
      src.events.filter(e => date.isWithinPeriod(Year(e.date)))
    )

    def events: produces[Seq[propertyEvent]] =
      src.properties.biMap[produces[Seq[propertyEvent]]](err => noVal(err.value: _*))(d =>
        someval(d.get.value.flatMap(_.value))
      )

    def properties: dataset[Properties] =
      if (src.isInstanceOf[Properties]) src else src.<--[Properties]
  }
  implicit class GrowRents[A <: Properties with Date](src: dataset[A])(implicit taga: TypeTag[A]) {
    def accrueRent: dataset[A] = src.+->[Properties]
  }
  implicit class PayRents[A <: Properties with Accounts with Date](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {

    def payRents: dataset[A] =
      for {
        properties <- src.properties
        accounts <- src.accounts
        date <- src.currentDate
        rentAcct <- accounts
          .collectFirst({ case c: CheckingAccount => c })
          .asInstanceOf[Option[Account]]
          .fromOption
          .fold(err => DatasetError[Account](new Error("No Rent account found") +: err.value: _*))(
            d => d
          )
      } yield {
        val rentPaymentsPaid = properties
          .eventsAtDate(date)
          .collect({
            case r: rentPaymentDue => rentPaymentPaid(r.propertyId, r.amount, r.date)
          })
        rentPaymentsPaid.foldLeft(src)((accumSrc, event) =>
          accumSrc.withdraw(rentAcct, event.amount)
        ) ++ properties.addEvent(rentPaymentsPaid: _*)
      }
  }

}

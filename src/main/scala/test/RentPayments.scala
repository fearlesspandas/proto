package test
import Typical.core.dataset._
import Typical.core.grammar._
import Account._
import EventHandler._
case class rentPaymentDue(amount:Double) extends Event
case object RentPayments extends (EventStore ==> EventStore){
  val rentPayments = Seq(rentPaymentDue(1300))
  override def apply(src: dataset[EventStore]): dataset[EventStore] = for{
    eventLog <- src.fetch[EventStore]
  }yield eventLog.addEvents(rentPayments:_*)
}

trait Property extends ::[Property]{
  val id:Long
}
case class RentalProperty(id:Long)
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
  def get(id:Long):dataset[Property] = propertyMap(id)
  def update(property:Property):dataset[Properties] = apply(property)
  //implicit class PropertiesAPI[](src:)
}

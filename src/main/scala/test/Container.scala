package test
import Typical.core.{dataset, grammar}
import grammar._
import dataset._
import test.Income.Incomes

trait Identifiable{
  val id:Long
}
trait Container[deps<:dataset[_],A<:(deps ==> A) with Identifiable,self<:Container[_,_,self]] extends produces[Seq[A]]  with (deps ==> self){
  protected def apply(coll:Map[Long,A]):dataset[self]
  def apply(elem:A):dataset[self] = {
    val updatedMap = stateMap.updated(elem.id,elem)
    this.apply(updatedMap)
  }
  private lazy val stateMap:Map[Long,A] = value.map(a => a.id -> a).toMap
  def get(id:Long):A
}

trait EventBased[E,self<:EventBased[_,self]]{
  def addEvent(events:E*):self
}

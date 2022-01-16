package test
import Typical.core.dataset._
import Typical.core.grammar._

import scala.reflect.runtime.universe.TypeTag
trait Identifiable {
  val id: Long
}

object Containers {
  trait BasicContainer[A <: dataset[A] with Identifiable, self <: BasicContainer[
    A,
    self
  ]] extends ::[self]
      with produces[Seq[A]] {
    private val stateMap: Map[Long, A] = value.map(a => a.id -> a).toMap

    def get(id: Long): A = stateMap(id)

    def update(elem: A): dataset[self] = apply(elem)

    protected def apply(elem: A): dataset[self] = {
      val updatedMap = stateMap.updated(elem.id, elem)
      this.apply(updatedMap)
    }

    protected def apply(coll: Map[Long, A]): dataset[self]

  }

  trait EventBasedBasicContainer[E <: Identifiable, A <: dataset[A] with Identifiable with EventBased[
    E,
    A
  ], self <: EventBasedBasicContainer[E, A, self]]
      extends BasicContainer[A, self]
      with EventBased[E, self] {
    implicit val tagself: TypeTag[self]
    implicit val taga: TypeTag[A]
    def addEvent(events: E*): dataset[self] =
      events.foldLeft(this) { (accumincs, e) =>
        val income = accumincs.get(e.id)
        accumincs.apply(income.addEvent(e).get).get
      }
  }
//  implicit class BasicContainerGram[A<:dataset[_]](src:dataset[A]){
//    def update[E<:dataset[_],U>:dataset[A]<:BasicContainer[E,U]](elem:E):dataset[A] = for{
//      t <- src.<--[U]
//    }yield src ++ t.update(elem)
//  }
}

trait ModelContainer[deps <: dataset[_], A <: (deps with self ==> A) with Identifiable, self <: ModelContainer[
  deps,
  A,
  self
]] extends produces[Seq[A]]
    with (deps ==> self) {
  implicit val tagself: TypeTag[self]
  implicit val taga: TypeTag[A]
  implicit val tagdeps: TypeTag[deps]
  private val stateMap: Map[Long, A] = value.map(a => a.id -> a).toMap

  def get(id: Long): A = stateMap(id)

  def apply(src: dataset[deps with self]): dataset[self] =
    for {
      models <- src.<--[self]
    } yield {
      models.value
        .foldLeft(src)((accumSrc, i: A) =>
          for {
            derivedModel <- accumSrc.++(i).<-+[A]
            currentModels <- accumSrc.<--[self]
            updatedModels <- currentModels.update(derivedModel)
          } yield accumSrc ++ updatedModels
        )
        .<--[self]
    }

  def update(elem: A): dataset[self] = apply(elem)

  protected def apply(elem: A): dataset[self] = {
    val updatedMap = stateMap.updated(elem.id, elem)
    this.apply(updatedMap)
  }

  protected def apply(coll: Map[Long, A]): dataset[self]
}

trait EventBased[E, +self <: dataset[self]] {
  def addEvent(events: E*): dataset[self]
}
trait EventLog[E, +self <: dataset[self]]
    extends produces[Seq[E]]
    with Identifiable
    with EventBased[E, self]

trait ModelEventLog[E, deps <: dataset[_], self <: ModelEventLog[E, deps, self]]
    extends (deps ==> self)
    with EventLog[E, self]

trait EventBasedModelContainer[E <: Identifiable, deps <: dataset[_], A <: (deps ==> A) with Identifiable with EventBased[
  E,
  A
], self <: EventBasedModelContainer[E, deps, A, self]]
    extends ModelContainer[deps, A, self]
    with EventBased[E, self] {
  def addEvent(events: E*): dataset[self] =
    events.foldLeft(this) { (accumincs, e) =>
      val income = accumincs.get(e.id)
      accumincs.apply(income.addEvent(e).get).get
    }
}

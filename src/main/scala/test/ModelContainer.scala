package test
import Typical.core.dataset._
import Typical.core.grammar._
import test.Date.Date
import test.Date.Year
import test.Event.Event

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

  trait EventBasedBasicContainer[A <: dataset[A] with Identifiable with EventBased[
    A
  ], self <: EventBasedBasicContainer[A, self]]
      extends BasicContainer[A, self]
      with EventBased[self]
      with produces[Seq[A]] {
    implicit val tagself: TypeTag[self]
    implicit val taga: TypeTag[A]
    def addEvent(events: (Event with Identifiable)*): dataset[self] =
      events.foldLeft(this) { (accumincs, e) =>
        val income = accumincs.get(e.id)
        accumincs.apply(income.addEvent(e).get).get
      } //todo make full replacement of events
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

  trait EventBased[+self <: dataset[self]] {
    def addEvent(events: (Event with Identifiable)*): dataset[self]
  }
  trait EventLog[+self <: dataset[self]]
      extends produces[Seq[Event]]
      with Identifiable
      with EventBased[self]

  implicit class EventLogGram[A <: produces[Iterable[Event]] with dataset[A]](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {
    def eventsAtDate(date: Date) = src.events.filter(e => date.isWithinPeriod(Year(e.date)))

    def events = self.get.value

    def self: dataset[A] = if (src.isInstanceOf[EventLog[_]]) src else src.<--[A]
  }
  trait ModelEventLog[deps <: dataset[_], self <: ModelEventLog[deps, self]]
      extends (deps ==> self)
      with EventLog[self]

  trait EventBasedModelContainer[deps <: dataset[_], A <: (deps ==> A) with Identifiable with EventBased[
    A
  ], self <: EventBasedModelContainer[deps, A, self]]
      extends ModelContainer[deps, A, self]
      with EventBased[self] {
    def addEvent(events: (Event with Identifiable)*): dataset[self] =
      events.foldLeft(this) { (accumincs, e) =>
        val income = accumincs.get(e.id)
        accumincs.apply(income.addEvent(e).get).get
      }
  }

  implicit class ModelEventContainerGram[A <: produces[_<:Iterable[produces[Seq[Event]]]] with dataset[A]](src: dataset[A])(implicit taga: TypeTag[A]) {
    def eventsAtDate(date: Date) = src.events.filter(e => date.isWithinPeriod(Year(e.date)))

    def events = self.get.value.flatMap(_.value)

    def self: dataset[A] =
      if (!src.isContext) src else src.<--[A]
  }

}

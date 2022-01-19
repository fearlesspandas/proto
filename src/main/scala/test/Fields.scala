package test

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account.AccountingBalanceEvent
import test.Account.AccountingCostBasisEvent
import test.Event.Event

import scala.reflect.runtime.universe.TypeTag
object Fields {

  case class Bal[A <: dataset[A] with produces[Seq[Event]]](
    value: Option[Double],
    events: Seq[Event] = Seq()
  )(
    implicit taga: TypeTag[A],
    tagself: TypeTag[Bal[A]]
  ) extends (A ==> Bal[A])
      with produces[Option[Double]] {
    override def apply(src: dataset[A]): dataset[Bal[A]] =
      for {
        model <- src.<--[A]
      } yield {
        val releventEvents = model.value.collect({ case b: AccountingBalanceEvent => b })
        val res =
          Bal[A](Some(releventEvents.net), releventEvents)
        res
      }
  }

  case class CosBas[A <: dataset[A] with produces[Seq[Event]]](value: Option[Double])(
    implicit taga: TypeTag[A],
    tagself: TypeTag[Bal[A]]
  ) extends (A ==> Bal[A])
      with produces[Option[Double]] {
    override def apply(src: dataset[A]): dataset[Bal[A]] =
      for {
        model <- src.<--[A]
      } yield {
        val res =
          Bal[A](Some(model.value.collect({ case b: AccountingCostBasisEvent => b }).net))
        res
      }
  }

  implicit class FieldGrammer[A <: dataset[A] with produces[Seq[Event]]](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {
    def bal: dataset[Bal[A]] = {
      val model = if (src.isContext) src.<--[A].get else src.get
      val ctx = data[A]() ++ model ++ (new Bal[A](None))
      ctx.<-+[Bal[A]]
    }
  }
}

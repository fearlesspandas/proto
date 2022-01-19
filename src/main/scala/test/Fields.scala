package test

import Typical.core.dataset._
import Typical.core.grammar._
import test.Account.AccountingBalanceEvent
import test.Account.AccountingCostBasisEvent
import test.Event.Event

import scala.reflect.runtime.universe.TypeTag
object Fields {

  case class Balance[A <: dataset[A] with produces[Seq[Event]]](
    value: Option[Double],
    events: Seq[Event] = Seq()
  )(
    implicit taga: TypeTag[A]
  ) extends (A ==> Balance[A])
      with produces[Option[Double]] {
    override def apply(src: dataset[A]): dataset[Balance[A]] =
      for {
        model <- src.<--[A]
      } yield {
        val releventEvents = model.value.collect({ case b: AccountingBalanceEvent => b })
        val res =
          Balance[A](Some(releventEvents.net), releventEvents)
        res
      }
  }
  //todo maybe make fields implicit classes?
  case class CostBasis[A <: dataset[A] with produces[Seq[Event]]](value: Option[Double])(
    implicit taga: TypeTag[A]
  ) extends (A ==> CostBasis[A])
      with produces[Option[Double]] {
    override def apply(src: dataset[A]): dataset[CostBasis[A]] =
      for {
        model <- src.<--[A]
      } yield {
        val res =
          CostBasis[A](Some(model.value.collect({ case b: AccountingCostBasisEvent => b }).net))
        res
      }
  }

  implicit class FieldGrammer[A <: dataset[A] with produces[Seq[Event]]](src: dataset[A])(
    implicit taga: TypeTag[A]
  ) {
    lazy val BALANCE: dataset[Balance[A]] =
      ctx.<-+[Balance[A]]
    lazy val COST_BASIS =
      ctx.<-+[CostBasis[A]]
    private val model = if (src.isContext) src.<--[A].get else src.get
    private val ctx = data[A]() ++ model ++ (new Balance[A](None))

  }
}

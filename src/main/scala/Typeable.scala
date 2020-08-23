package Typical.core;

import scala.reflect.{ClassTag, classTag}

import scala.collection.immutable.HashMap

package object Typeable {
  import grammar._
  def build[A](implicit tag:ClassTag[A]) = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  type contexttype = Map[Any,Any]
  type idtype = String

  trait dataset[+A <: dataset[_]]{
    val context:contexttype
    def withContext(ctx:contexttype):dataset[A]
    val id:idtype
  }

  trait axiom[A <: axiom[A]] extends dataset[A]{
    val value:Any = null
    override val id = this.getClass.getSimpleName
    override val context: contexttype = Map()
  }

  trait model[-dependencies <: dataset[_], output <: model[_,output]] extends dataset[output] {
    val iterate: dataset[dependencies] => output
    val value:Any = null
    override val id = this.getClass.getSimpleName
    override val context: contexttype = Map()
  }


  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = ""
    def dataset = this.asInstanceOf[dataset[A]]
  }

  class thing extends model[thing,thing] {
    override val iterate: dataset[thing] => thing = (src:dataset[thing]) => {
      new thing {
        override val value = src.fetchAs[thing,Int] + 10
      }
    }
    override def withContext(ctx: contexttype): thing = new thing {
      override val context = ctx
    }
  }
  class othertype extends model[thing,othertype] {
    override val iterate: dataset[thing] => othertype =
      (src:dataset[thing]) => {
      new othertype {
        override val value = src.fetchAs[thing,Int] + 5
      }
    }
    override def withContext(ctx: contexttype): dataset[othertype] = new othertype {
      override val context = ctx
    }
  }

}

package object grammar {
  import Typeable._
  implicit class Calcer[A<:dataset[_]](a:dataset[A]){
    def calc[U<:model[A,U]](implicit tag:ClassTag[U]):dataset[A with U] = {
      val instA = build[U]
      a.withContext(a.context.updated(instA.id,instA.iterate(a).value))
        .asInstanceOf[dataset[A with U]]
    }
  }
  implicit class Fetcher[A<:dataset[_]](a:dataset[A]){
    def fetchAs[U>:A<:dataset[U],tpe](implicit tag:ClassTag[U]) = a.context.get(build[U].id).collect({case a:tpe => a}).get
    def fetch[U>:A<:dataset[U]](implicit tag:ClassTag[U]) = a.context.get(build[U].id).get
  }
  implicit class ContextBuilder(m:Map[Any,Any]){
    def register[U<:dataset[_]](value:Any)(implicit tag:ClassTag[U]):Map[Any,Any] = m.updated(build[U].id,value)
  }
  implicit class Includer[A<:dataset[_]](a:dataset[A]){
    def include[U<:dataset[_]](value:Any)(implicit tag:ClassTag[U]) = {
      val instance = build[U]
      a.withContext(a.context.updated(instance.id,value))
        .asInstanceOf[dataset[A with U]]
    }
  }
}

object runner {
  import Typeable._
  import grammar._
  def main(args:Array[String]):Unit = {
    val test = data[othertype](
      Map[Any,Any]()
        .register[thing](1)
        .register[othertype](2)
    )
      .include[thing](100)
      .calc[othertype]
      .calc[thing]
      .calc[othertype]
      val res = test.fetchAs[othertype,Int]
    println(res)
  }
}
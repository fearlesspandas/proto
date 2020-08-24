package Typical.core;

import scala.reflect.{ClassTag, classTag}

import scala.collection.immutable.HashMap

package object Typeable {
  import grammar._
  private[core] def build[A](implicit tag:ClassTag[A]) = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
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
    def iterate(src:dataset[dependencies]):output
    val value:Any = null
    override val id = this.getClass.getSimpleName
    override val context: contexttype = Map()
  }

  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = ""
    def dataset = this.asInstanceOf[dataset[A]]
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
    def include[U<:dataset[_]](value:Any)(implicit tag:ClassTag[U]):dataset[A with U] = {
      val instance = build[U]
      a.withContext(a.context.updated(instance.id,value))
        .asInstanceOf[dataset[A with U]]
    }
  }
  implicit class Merger[A<:dataset[A]](a:dataset[A]){
    def merge[B<:dataset[_]](b:dataset[B]):Option[dataset[A with B]] = {
      val matchkeys = a.context.keys.filter(k => b.context.keys.toSeq.contains(k))
      val noContradiction = matchkeys.foldLeft(true)( (acc,key) => {
        acc && b.context.get(key) == a.context.get(key)
      })
      noContradiction match {
        case false => None
        case true =>
          Some(
            a.withContext(a.context ++ b.context).asInstanceOf[dataset[A with B]]
          )

      }
    }
  }
}

object runner {
  import Typeable._
  import grammar._
  class thing extends model[thing with othertype,thing] {
    override def iterate(src:dataset[thing with othertype]):thing = {
      new thing {
        override val value = src.fetchAs[thing,Int] + 10
      }
    }
    override def withContext(ctx: contexttype): thing = new thing {
      override val context = ctx
    }
  }
  class othertype extends model[thing,othertype] {
    override def iterate(src:dataset[thing]): othertype =
      new othertype {
        override val value = src.fetchAs[thing,Int] + 5
      }

    override def withContext(ctx: contexttype): dataset[othertype] = new othertype {
      override val context = ctx
    }
  }

  class equiv[A<:model[_<:dataset[B],A],B<:model[_<:dataset[A],B]] extends model[A with B,equiv[A,B]]{
    //override val id = this.getClass.getSimpleName + build[A].id + build[B].id
    override def iterate(src: dataset[A with B]): equiv[A, B] = new equiv[A,B] {
      override val value: Any = true
    }
    override def withContext(ctx: contexttype): dataset[equiv[A, B]] = new equiv[A,B] {
      override val context: contexttype = ctx
    }
  }
  def main(args:Array[String]):Unit = {
    val test = data[othertype](
      Map[Any,Any]()
        .register[othertype](1)
    )
    val dat = data[thing](
        Map[Any,Any]()
          .register[thing](100)
        )
    val merged = test.merge(dat)
      //.calc[equiv[othertype,thing]]
      val res = merged.get.context//.fetchAs[equiv[othertype,thing],Boolean]
    println(res)
  }
}
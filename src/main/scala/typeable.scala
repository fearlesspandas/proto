package Typical.core;

import scala.reflect.{ClassTag, classTag}

import scala.collection.immutable.HashMap

package object typeable {
  import grammar._
  private[core] def build[A](implicit tag:ClassTag[A]) = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  type contexttype = Map[Any,Any]
  type idtype = String

  trait dataset[+A <: dataset[_]]{
    val context:contexttype
    def withContext(ctx:contexttype):dataset[A]
    def id:idtype
    val value:Any = null
  }

  trait axiom[A <: axiom[A]] extends dataset[A]{
    override val value:Any = null
    override val id = this.getClass.getSimpleName
    override val context: contexttype = Map()

    override def withContext(ctx: contexttype): dataset[A] = null
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] {
    def iterate(src:dataset[dependencies]):output
    override val value:Any = null
    override val id = this.getClass.getSimpleName
    override val context: contexttype = Map()

    override def withContext(ctx: contexttype): dataset[output] = null
  }

  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = null
    def dataset = this.asInstanceOf[dataset[A]]
  }


}

package object grammar {
  import typeable._
  implicit class Calcer[A<:dataset[_]](a:dataset[A]){
    def calc[U<:model[A,U]](implicit tag:ClassTag[U]):dataset[A with U] = {
      val instA = build[U]
      a.withContext(a.context.updated(instA.id,instA.iterate(a).value))
        .asInstanceOf[dataset[A with U]]
    }
    def calcFor[U<:model[A,B],B<:dataset[_]](implicit tag:ClassTag[U],tagb:ClassTag[B]):dataset[A with B] = {
      val instU = build[U]
      val instB = build[B]
      a.withContext(a.context.updated(instB.id,instU.iterate(a).value))
        .asInstanceOf[dataset[A with B]]
    }

  }
  implicit class Fetcher[A<:dataset[_]](a:dataset[A]){
    def fetchAsOpt[U>:A<:dataset[U],tpe](implicit tag:ClassTag[U]):Option[tpe] = a.context.get(build[U].id).collect({case a:tpe => a})
    def fetchAs[U>:A<:dataset[U],tpe](implicit tag:ClassTag[U]):tpe = a.context.get(build[U].id).collect({case a:tpe => a}).get
    def fetch[U>:A<:dataset[U]](implicit tag:ClassTag[U]) = a.context.get(build[U].id).get
  }
  implicit class ContextBuilder(m:Map[Any,Any]){
    def register[U<:dataset[_]](value:Any)(implicit tag:ClassTag[U]):Map[Any,Any] = m.updated(build[U].id,value)
    def remove[U<:dataset[_]](implicit tag:ClassTag[U]):Map[Any,Any] = m.toSeq.filterNot( p => p._1 == build[U].id).toMap
    //.filterNot( (k,v) =>  (k == build[U].id))
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
    def induct[B<:dataset[_]]() = ???
  }

  implicit class Iterator[A<:dataset[_]](a:dataset[A]){
    def iter[F<:dataset[A] => dataset[A]](f:F):dataset[A] = f(a)
    def transact[U>:A<:dataset[_]](f:dataset[A] => dataset[U]):dataset[U] = f(a)
  }


}

object runner {
  import typeable._
  import grammar._
  class thing extends model[thing with otherthing,thing] {
    override def iterate(src:dataset[thing with otherthing]):thing = new thing {
        override val value = src.fetchAsOpt[thing,Int].get + 10
      }

  }
  class otherthing extends model[thing,otherthing] {
    override def iterate(src:dataset[thing]): otherthing = new otherthing {
        override val value = src.fetchAs[thing,Int] + 5
      }
  }

  class testhing extends model[thing,otherthing]{
    override def iterate(src: dataset[thing]): otherthing = new otherthing{
      override val value = 1000000000
    }
  }
  def f(th:dataset[thing with otherthing]) : dataset[thing] = th.withContext(th.context.remove[otherthing])

  class equiv[A<:model[_<:dataset[B],A],B<:model[_<:dataset[A],B]] extends model[A with B,equiv[A,B]]{
    override def iterate(src: dataset[A with B]): equiv[A, B] = new equiv[A,B] {
      override val value: Any = true
    }
  }
  def main(args:Array[String]):Unit = {
    val test = data[otherthing](
      Map[Any,Any]()
        .register[otherthing](1)
    )
    val dat = data[thing](
        Map[Any,Any]()
          .register[thing](100)
        )
    val merged = test.merge(dat)
    val test2 = data[otherthing with thing with equiv[thing,otherthing]](
      Map[Any,Any]()
        .register[thing](1)
    )
    val t = test2.calc[thing].calcFor[testhing,otherthing].calc[otherthing]
    println(t.context)
    //val t1 = test2.transact[thing]
      //.calc[equiv[othertype,thing]]
      val res = merged.get.context
    println(res)
  }
}
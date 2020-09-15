package Typical.core;


import scala.reflect._
import scala.collection.immutable.HashMap
import scala.reflect.runtime.universe._
package object typeable {



  def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    println(s"type of $x has type arguments $targs")
  }
  def getType[T:TypeTag](t:T) = typeTag[T]
  val thing = getType(data(null)).tpe.decls.take(10)

//  implicit class Builder[A<:dataset[_<:datastore[_]]](a:dataset[A]){
//    def to
//  }
  //case class buildhelper[A](id:idtype,iterate:)a
  //private[core] def buildFunc[A<:model[dependencies,A],dependencies](implicit tag:ClassTag[A]) = new (model[dependencies,A] with dataset[A])//classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  //private[core] def buildId[A<:dataset[_]] = dataset[A].id
  private[core] def build2[A](implicit tag:ClassTag[A],ttag:TypeTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  private[core] def build[A:TypeTag]:A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    val c2 = cm.reflectConstructor(c1)
    c2().asInstanceOf[A]
  }
  type contexttype = Map[Any,dataset[_]]
  type idtype = String


  trait InitialType[+T]{
    val typedValue:T = null.asInstanceOf[T]
  }
  trait dataset[+A <: dataset[_]]{
    val context:contexttype
    def withContext(ctx:contexttype):dataset[A]
    def id:idtype
    val value:Any = null
  }

  trait axiom[A <: axiom[A]] extends dataset[A]{
    override val value:Any = null
    override final val id = this.getClass.getSimpleName
    override val context: contexttype = Map()

    override def withContext(ctx: contexttype): dataset[A] = null
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] {
    def iterate(src:dataset[dependencies]):output = null.asInstanceOf[output]
    override val value:Any = null
    override final val id = this.getClass.getSimpleName
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
    def calc[U<:model[A,U]](implicit ttag:TypeTag[U]):dataset[A with U] = {
      val instA = build[U]
      a.withContext(a.context.updated(instA.id,instA.iterate(a)))
        .asInstanceOf[dataset[A with U]]
    }
    def calcAs[U<:model[A,U] with InitialType[tpe],tpe](implicit ttag:TypeTag[U]):dataset[A with U] = {
      val instA = build[U]
      a.withContext(a.context.updated(instA.id,instA.iterate(a)))
        .asInstanceOf[dataset[A with U]]
    }
//    def calcFor[U<:model[A,B],B<:datastore[_]](implicit tag:ClassTag[U], tagb:ClassTag[B]):datastore[A with B] = {
//      val instU = buildFunc[U,A]
//      val instB = buildFunc[B]
//      a.withContext(a.context.updated(instB.id,instU.iterate(a)))
//        .asInstanceOf[datastore[A with B]]
//    }

  }
  implicit class Fetcher[A<:dataset[_]](a:dataset[A]){
    def fetchAs[U>:A<:dataset[U] with InitialType[tpe],tpe](implicit ttag:TypeTag[U]):Option[tpe] = a.context.get(build[U].id) match {
      case Some(d:U) => Some(d.typedValue)
      case _ => None
    }
    def fetch[U>:A<:dataset[U]](implicit ttag:TypeTag[U]):Option[U] = a.context.get(build[U].id).asInstanceOf[Option[U]]
  }
  implicit class ContextBuilder(m:Map[Any,Any]){
    def register[U<:dataset[_]](value:U)(implicit ttag:TypeTag[U]):contexttype = m.updated(build[U].id,value).asInstanceOf[contexttype]
    def remove[U<:dataset[_]](implicit ttag:TypeTag[U]):contexttype = m.toSeq.filterNot(p => p._1 == build[U].id).toMap.asInstanceOf[contexttype]

  }
  implicit class ContextViewer(m:Map[Any,dataset[_]]){
    def valueView():Map[Any,Any] = m.map( p =>
    {
      val k = p._1
      val v:dataset[_] = p._2
      val prettyval = if(v.isInstanceOf[InitialType[_]]) v.asInstanceOf[InitialType[_]].typedValue
      else v.value
      k -> prettyval
    })
  }

  implicit class Includer[A<:dataset[_]](a:dataset[A]){
    def include[U<:dataset[_]](value:U)(implicit ttag:TypeTag[U]):dataset[A with U] = {
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
    def run[F<:dataset[A] => dataset[B] with InitialType[tpe],B<:dataset[A],tpe](f:F):dataset[B] = a.withContext(a.context.updated(f(a).id,f(a))).asInstanceOf[dataset[B]]
  }




}

object runner {
  import typeable._
  import grammar._

  case class input(thing1:Seq[Double],thing2:Seq[Double])
  class Input extends axiom[Input] with InitialType[input]

  class checkOne extends model[Input,checkOne] with InitialType[Seq[Double]]{
    override def iterate(src:dataset[Input]):checkOne = new checkOne {
        override val typedValue = {
          val in = src.fetchAs[Input,input].get
          in.thing1.filter(_<9)
        }
      }
  }
  abstract class checkTwo[A<: dataset[A] with InitialType[Seq[Double]]](implicit tag:TypeTag[A]) extends model[A,A] with InitialType[Seq[Double]] {
      override def iterate(src: dataset[A]): A = new checkTwo[A] {
        override val typedValue = src.fetchAs[A, Seq[Double]].get.distinct
      }.asInstanceOf[A]
  }
  case class Thing1() extends checkTwo[checkOne]



  def main(args:Array[String]):Unit = {
    val dat = data[Input](
        Map[Any,dataset[_]]()
          .register[Input](
            new Input {
              override val typedValue: input = input(Seq(11,1,1,1,1),Seq(2,2,2,2))
            }
          )
        )
    val res = dat.calc[checkOne].calc[Thing1]//run(runnerfunc(_))
    println(res.context.valueView())
    //println()
  }
}
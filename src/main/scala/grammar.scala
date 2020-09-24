package Typical.core
import scala.reflect.runtime.universe._
package object grammar {
  import typeable._
  implicit class Calcer[A<:dataset[_]](a:dataset[A]){

    def calc[U<:model[A,U]](implicit ttag:TypeTag[U],atag:TypeTag[A]):dataset[A with U] = {
      val instA = build[U]
      if (a.withContext(Map()) == null) throw new Error(contextErrorStringCalc(build[A].id))
      else a.withContext(a.context.updated(instA.id,instA.iterate(a)))
        .asInstanceOf[dataset[A with U]]
    }
    def calcInferred[U<:model[A,U]](u:U)(implicit atag:TypeTag[A]):dataset[A with U] = {
      if (a.withContext(Map()) == null) throw new Error(contextErrorStringCalc(build[A].id))
      else a.withContext(a.context.updated(u.id,u.iterate(a)))
        .asInstanceOf[dataset[A with U]]
    }
    def calcAs[U<:model[A,U] with InitialType[tpe],tpe](implicit ttag:TypeTag[U],atag:TypeTag[A]):dataset[A with U] = {
      val instA = build[U]
      if (a.withContext(Map()) == null) throw new Error(contextErrorStringCalc(build[A].id))
      else a.withContext(a.context.updated(instA.id,instA.iterate(a)))
        .asInstanceOf[dataset[A with U]]
    }
    def calcFor[U<:model[A,B],B<:dataset[_]](implicit tag:TypeTag[U], tagb:TypeTag[B],atag:TypeTag[A]):dataset[A with B] = {
      val instU = build[U]
      val instB = build[B]
      if (a.withContext(Map()) == null) throw new Error(contextErrorStringCalc(build[A].id))
      else a.withContext(a.context.updated(instB.id,instU.iterate(a)))
        .asInstanceOf[dataset[A with B]]
    }
  }
  implicit class Fetcher[A<:dataset[_]](a:dataset[A]){
    def fetchAs[U>:A<:dataset[_] with InitialType[tpe],tpe](implicit ttag:TypeTag[U],atag:TypeTag[A]):Option[tpe] =
      if(a.context.isEmpty) throw new Error(contextErrorStringFetch(build[A].id))
      else a.context.get(build[U].id) match {
        case Some(d:U) => Some(d.value)
        case _ => None
    }
    def fetch[U>:A<:dataset[U]](implicit ttag:TypeTag[U],atag:TypeTag[A]):Option[U] =
      if(a.context.isEmpty) throw new Error(contextErrorStringFetch(build[A].id))
      else (a.context.get(build[U].id) match {
        case Some(d:U) if d.withContext(a.context) == null => Some(d.asInstanceOf[U])
        case Some(d:U)  => Some(d.asInstanceOf[U].withContext(a.context))
        case _ => None
    }).asInstanceOf[Option[U]]
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
      val prettyval = if(v.isInstanceOf[InitialType[_]]) v.asInstanceOf[InitialType[_]].value
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
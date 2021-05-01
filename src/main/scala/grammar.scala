package Typical.core

import scala.reflect.runtime.universe._
package object grammar {
  import dataset._
  //implicit class ddddd[A<:dataset[_],B<:dataset[_]](src:dataset[A with B]){
    def remove[U<:dataset[_],A<:dataset[_]](src:dataset[A with U],u:U): dataset[A] = null
  //}
  implicit class AAA[B<:axiom[B]](m:dataset[B]){
    def flatMap[C<:dataset[_]](f:B=> dataset[C]):dataset[C] = if (m.isEmpty){
      throw m.asInstanceOf[DatasetError[B]].value
    } else f(m.asInstanceOf[B])

    def map[C<:dataset[C]](f:B => dataset[C]):dataset[C] = if (m.isEmpty) throw m.asInstanceOf[DatasetError[B]].value else f(m.asInstanceOf[B])
  }
  implicit class FFFF[B<:model[_<:dataset[_],B]](m:dataset[B]){
    def flatMap[C<:dataset[_]](f:B=> dataset[C]):dataset[C] = if (m.isEmpty){
      throw m.asInstanceOf[DatasetError[B]].value
    } else f(m.asInstanceOf[B])
    //def collect[U](partialFunction: PartialFunction[B,U]):dataset[B] = null
    def map[C<:dataset[C]](f:B => dataset[C]):dataset[C] = if (m.isEmpty) throw m.asInstanceOf[DatasetError[B]].value else f(m.asInstanceOf[B])
  }
  implicit class Calcer[A <: dataset[_]](src: dataset[A]) {

    def calc[U <: model[A, U]](
                                implicit ttag: TypeTag[U]
                              ): dataset[A with U] = {
      if (src.withContext(Map()) == null) throw new Error(s"override ${src.id}'s withContext method to use it as a data context'")
      else
        src.derive[U].flatMap((nextU:U) => src.include[U](nextU))
    }

    def derive[U <: model[A, U]](implicit utag:TypeTag[U]):dataset[U] = for {
      ustate <- src.asInstanceOf[dataset[A with U]].fetch[U]
      res <- ustate.iterate(src)
    }yield res



    def run[U <: model[A , _ >: dataset[A] <: dataset[_]]](
                                                            implicit ttag: TypeTag[U]
                                                          ): dataset[A] = {
      val instU = src.asInstanceOf[dataset[A with U]].multifetch[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(src.id))
      else
        instU match {
          case e:DatasetError[_] => throw e.value
          case _ =>
            val b = instU.asInstanceOf[U].iterate(src)
            src.withContext(src.context ++ b.context)
        }
    }
  }
  implicit class Fetcher[A <: dataset[_]](a: dataset[A]) {

    private[grammar] def multifetch[U >: A <: dataset[_]](implicit ttag: TypeTag[U]): dataset[U] =
      if (a.isEmpty) throw a.asInstanceOf[DatasetError[A]].value
      else {
        val uid = buildId[U]
        if (uid.toString().contains("trait")) {
          a.context.get(uid).flatMap(d => if (d.isInstanceOf[Id[_]]) a.context.get(d.asInstanceOf[Id[U]].baseId) else None) match {
            case Some(d: U) if d != null && d.isInstanceOf[U] => apply[U](d)
            case _ => DatasetError[U](new Error(s"No value for ${uid} found for fetch[$uid]"),uid)
          }
        }
        else
        a.context.get(uid) match {
          case Some(d: U) if d != null && d.isInstanceOf[U] => apply[U](d)
          case _ => DatasetError[U](new Error(s"No value for ${uid} found for fetch[$uid]"),uid)
        }
      }
    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U]): dataset[U] =
      if (a.isEmpty) throw a.asInstanceOf[DatasetError[A]].value
      else a.multifetch[U]
  }
  implicit class ContextBuilder(m: Map[idtype, dataset[_]]) {
    def register[U <: dataset[_]](value:U)(implicit ttag: TypeTag[U]): contexttype = {
      val id = buildId[U]
      m.updated(buildId[U], value).asInstanceOf[contexttype]
    }
    def bind[U <: dataset[_],T<:dataset[_] with Id[T]](value:T)(implicit ttag: TypeTag[U]): contexttype = {
      val id = buildId[U]
      m.updated(buildId[U], value).asInstanceOf[contexttype]
    }

    def remove[U <: dataset[_]](implicit ttag: TypeTag[U]): contexttype =
      m.toSeq.filterNot(p => p._1 == buildId[U]).toMap.asInstanceOf[contexttype]
  }
  implicit class Includer[A <: dataset[_]](a: dataset[A]) {
    def include[U <: dataset[_]](value: dataset[U])(implicit ttag: TypeTag[U]): dataset[A with U] = {
      val res = a.withContext(a.context.updated(buildId[U], value))
      res.asInstanceOf[dataset[A with U]]
    }

  }
  implicit class Merger[A <: dataset[A]](a: dataset[A]) {
    def merge[B <: dataset[_]](b: dataset[B]): dataset[A with B] = {
      val matchkeys = a.context.keys.filter(k => b.context.keys.toSeq.contains(k))
      val noContradiction = matchkeys.foldLeft(true)((acc, key) => {
        acc && b.context.get(key) == a.context.get(key)
      })
      noContradiction match {
        case false => null//error here
        case true =>
          a.withContext(a.context ++ b.context).asInstanceOf[dataset[A with B]]
      }
    }
    def induct[B <: dataset[_]]() = ???
  }


}

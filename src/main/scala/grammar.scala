package Typical.core

import scala.reflect.runtime.universe._

package object grammar {

  import dataset._

  //implicit class ddddd[A<:dataset[_],B<:dataset[_]](src:dataset[A with B]){
  def remove[U <: dataset[_], A <: dataset[_]](src: dataset[A with U], u: U): dataset[A] = null

  //}


//  implicit class ImplicitGram[A <: dataset[implicitModel[A, out]], out <: model[A, out]](a: dataset[A])(implicit tagout: TypeTag[out]) {
//    def implicitFetch2[U <: model[A, out]](implicit tagdep: TypeTag[U], tagA: TypeTag[A]): dataset[out] = {
//      val idl = buildId[A]
//      val idr = buildId[out]
//      val res = a.context.values.filter({
//        case u: implicitModel[dep, out] if idLess(u.id_l, idl) && u.id_r == idr => true;
//        case _ => false
//      }).headOption match {
//        case Some(o: model[_, _]) => o.asInstanceOf[model[U, out]]
//        case _ => DatasetError[out](new Error(s"no implicit value found for $idl -> $idr"))
//      }
//      res
//    }
//  }

  implicit class AAA[B <: axiom[B]](m: dataset[B]) {
    def flatMap[C <: dataset[_]](f: B => dataset[C]): dataset[C] = if (m.isEmpty) {
      val lasterr = m.asInstanceOf[DatasetError[B]].value
      DatasetError[C](new Error(s"error while processing FlatMap")).append(lasterr: _*)
    } else f(m.asInstanceOf[B])

    def map[C <: dataset[C]](f: B => dataset[C]): dataset[C] = if (m.isEmpty) {
      val lasterr = m.asInstanceOf[DatasetError[B]].value
      DatasetError[C](new Error(s"error while processing map")).append(lasterr: _*)
    } else f(m.asInstanceOf[B])


  }

  implicit class FFFF[B <: model[_ <: dataset[_], B]](m: dataset[B])(implicit tagb: TypeTag[B]) {
    def flatMap[C <: dataset[_]](f: B => dataset[C])(implicit tagC: TypeTag[C]): dataset[C] = if (m.isEmpty) {
      val lasterr = m.asInstanceOf[DatasetError[B]].value
      DatasetError[C](new Error(s"error while processing flatMap ${buildId[C]}")).append(lasterr: _*)
    } else f(m.asInstanceOf[B])

    def map[C <: dataset[C]](f: B => dataset[C])(implicit tagc: TypeTag[C]): dataset[C] = if (m.isEmpty) {
      val lasterr = m.asInstanceOf[DatasetError[B]].value
      DatasetError[C](new Error(s"error while processing map ${buildId[C]}")).append(lasterr: _*)
    } else f(m.asInstanceOf[B])

  }

  implicit class Calcer[A <: dataset[_]](src: dataset[A]) {

    def calc[U <: Function1[dataset[A], dataset[U]] with dataset[U]](
                                implicit ttag: TypeTag[U],
                                tagA: TypeTag[A]
                              ): dataset[A with U] = {
      if (src.isEmpty) {
        DatasetError[A with U](new Error(s"Error while doing calc ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      }
      else
        src.derive[U].fold(
          e => DatasetError[A with U](new Error(s"Failure to calc ${buildId[U]}")).append(e.value: _*)
        )(
          (nextU: dataset[U]) => src.include[U, U](nextU.asInstanceOf[U])
        )
    }

    def derive[U <: Function1[dataset[A], dataset[U]] with dataset[U]](implicit utag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if (src.isEmpty) DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      else
        src.asInstanceOf[dataset[A with U]].multifetch[U].fold(e => DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(e.value: _*))(ustate => ustate.asInstanceOf[U].apply(src))
    }

    def run[U <: model[A, _ >: dataset[A] <: dataset[_]]](
                                                           implicit ttag: TypeTag[U],
                                                           tagA: TypeTag[A]
                                                         ): dataset[A] =
      if (src.isEmpty) src
      else
        src.asInstanceOf[dataset[A with U]].multifetch[U].fold(
          e => DatasetError[A](new Error(s"Failure to run ${buildId[U]}")).append(e.value: _*)
        )(instU => {
            instU.asInstanceOf[U].apply(src).fold(
              e => DatasetError[A](new Error(s"Failure to run ${buildId[U]}")).append(e.value: _*)
            )(
              b => src.withContext(src.context ++ b.context)
            )
        })
  }

  implicit class Fetcher[A <: dataset[_]](src: dataset[A]) {

    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] =
      src.multifetch[U]

    private[grammar] def multifetch[U >: A <: dataset[_]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if(src.isEmpty) DatasetError[U](new Error(s"Error while performing fetch[${buildId[U]}]")).append(src.asInstanceOf[DatasetError[A]].value:_*)
      else {
        val uid = buildIdLor[U](src.relations)
        src.context.get(uid) match {
          case Some(d: U) if d != null && d.isInstanceOf[U] =>
            apply[U](d)
          case _ => DatasetError[U](new Error(s"No value for ${buildId[U]} found for fetch[$uid]"))
        }
      }
    }

//    def implicitFetch[U <: model[A, out], out <: dataset[_]](implicit tagdep: TypeTag[U], tagout: TypeTag[out], tagA: TypeTag[A]): dataset[out] = {
//      val idl = buildId[A]
//      val idr = buildId[out]
//      val res = a.context.values.filter({
//        case u: implicitModel[dep, out] if idLess(u.id_l, idl) && u.id_r == idr => true;
//        case _ => false
//      }).headOption match {
//        case Some(o: model[_, _]) => o.asInstanceOf[model[U, out]]
//        case _ => DatasetError[out](new Error(s"no implicit value found for $idl -> $idr"))
//      }
//      res
//    }


  }

  implicit class ContextBuilder(m: Map[idtype, dataset[_]]) {
    def register[U <: dataset[_]](value: U)(implicit ttag: TypeTag[U]): contexttype = {
      val id = buildId[U]
      m.updated(buildId[U], value).asInstanceOf[contexttype]
    }

    def remove[U <: dataset[_]](implicit ttag: TypeTag[U]): contexttype =
      m.toSeq.filterNot(p => p._1 == buildId[U]).toMap.asInstanceOf[contexttype]
  }

  implicit class Includer[A <: dataset[_]](a: dataset[A]) {
    def include[U <: dataset[_], T <: U](value: T)(implicit ttag: TypeTag[U], tag: TypeTag[T]): dataset[A with U with T] = {
      if (value.isEmpty) DatasetError[A with U with T]()
      else {
        val newRelations = a.relations.updated(buildIdLor[U](a.relations), buildIdLor[T](a.relations))
        val newContext = a.context.updated(buildIdLor[U](newRelations), value)
        a.withRelations(newRelations).withContext(newContext)
          .asInstanceOf[dataset[A with U with T]]
      }

    }
  }

  implicit class Merger[A <: dataset[A]](a: dataset[A])(implicit taga: TypeTag[A]) {
    def merge[B <: dataset[_]](b: dataset[B])(implicit tagb: TypeTag[B]): dataset[A with B] = {
      val matchkeys = a.context.keys.filter(k => b.context.keys.toSeq.contains(k))
      val noContradiction = matchkeys.foldLeft(true)((acc, key) => {
        acc && b.context.get(key) == a.context.get(key)
      })
      noContradiction match {
        case false => new DatasetError[A with B](new Error(s"dataset ${buildId[A]} and ${buildId[B]} could not be merged"))
        case true =>
          a.withContext(a.context ++ b.context).asInstanceOf[dataset[A with B]]
      }
    }
  }


}

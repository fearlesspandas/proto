package Typical.core
import Grammar.Calc
import Grammar.FlatMap
import Grammar.TMap

import scala.reflect.runtime.universe._
package object grammar {
  import typeable._
  implicit class CalcerOp[A<:dataset[_]](src:Option[dataset[A]]){
    def calc[U<:model[A,U]](implicit ttag:TypeTag[U],atag:TypeTag[A]):Option[dataset[A with U]] = {
      src.flatMap(_.calc[U])
    }
    def calc[U<:axiom[U,T],T](f:dataset[A] => (T,U))(implicit tagU:TypeTag[U]) = {
      val t = new MmodelInstance[A,T,U](f,null.asInstanceOf[T])
      src.flatMap(t.iterate(_))
    }
    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], atag: TypeTag[A]): Option[U] = src.flatMap(_.fetch[U])
  }
  implicit class Calcer[A <: dataset[_]](src: dataset[A]) {
    /*
      Takes a model U with dependency set A that produces a U. Returns a (possibly expanded) dataset with both A and U
      ex, A,B,C are datasets, X is a model[A with B with C,X], and dataset[A with B with C].calc[X] will return
      a dataset[A with B with C with X]. If X is either A,B or C then an updated dataset of type dataset[A with B with C]
      will be returned
     */
    def calc[U <: model[A, U]](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): Option[dataset[A with U]] = {
      val uid = buildId[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
      else {
        val uState = src.context
          .get(uid)
          .map { case u: U if u != null => u }
          .getOrElse(new Error(s"Error while processing calc[${uid}]"))
          .asInstanceOf[U]
        val nextU = uState.iterate(src)
        if (nextU.isDefined) {
          Some(
            src
              .withContext(src.context.updated(uid, nextU.get))
              .asInstanceOf[dataset[A with U]]
          )
        } else throw new Error(s"Error while processing calc[${uid}]")
      }
    }
    def calcT[U <: model[A, U]](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): dataset[A with U with Calc[A, U]] = {
      val uid = buildId[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
      else {
        val uState = src.context
          .get(uid)
          .map { case u: U if u != null => u }
          .getOrElse(new Error(s"Error while processing calc[${uid}]"))
          .asInstanceOf[U]
        val nextU = uState.iterate(src)
        if (nextU.isDefined) {
          val c = Calc.apply[A, U](src, nextU.get)
          c.withContext(src.context.updated(uid, nextU.get))
            .asInstanceOf[Calc[A, U]]
          //.asInstanceOf[dataset[A with U]]
        } else throw new Error(s"Error while processing calc[${uid}]")
      }
    }
    /*
      Takes a model U with dependency set A, and returns a dataset[A] with an updated value for the output
      of U. This method is similar to calc except in the following behavior:
        1.calc allows for the ability to expand a dataset[A] to a dataset[A with B], whereas map will always
          return a dataset[A]
        2.map allows for the model U to have an output that is not of type U, whereas calc requires that U models type U.
          i.e. U can be of type model[A with B, C] where C != U, and calling dataset[A with B with C].map[U] will return
          a dataset[A with B with C] with an updated value for C.
     */
    def map[U <: model[A , _ >: dataset[A] <: dataset[_]]](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): Option[dataset[A]] = {
      val uid = buildId[U]
      val instU = src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it as a mapping"))
        .asInstanceOf[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
      else {
        val b = instU.iterate(src)
        if (b.isDefined) {
          Some(src.withContext(src.context.updated(b.get.id, b.get)))
        } else throw new Error(s"Error while processing map[$uid]")
      }
    }
    def mapT[U <: model[A, _ >: dataset[A] <: dataset[_]]](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): dataset[A with TMap[A, U]] = {
      val uid = buildId[U]
      val instU = src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it as a mapping"))
        .asInstanceOf[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
      else {
        val b = instU.iterate(src)
        if (b.isDefined) {
          val m = TMap.apply[A, U](src, instU)
          m.withContext(src.context.updated(b.get.id, b.get))
        }.asInstanceOf[TMap[A, U]]
        else throw new Error(s"Error while processing map[$uid]")
      }
    }
    /*
      Same thing as the calc method, except U's model output (of type U) also has its local context updated to match that of
      parent dataset that's calling it. WARNING: this requires any defined models that this is called on to have an overridden
      definition for the 'withContext' method on dataset[_], otherwise this will result in undefined behavior
     */
    def calcFullContext[U <: model[A, U]](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): dataset[A with U] = {
      val uid = buildId[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
      else {
        val uState = src.context
          .get(uid)
          .map { case u: U if u != null => u;}
          .getOrElse(throw new Error(s"trouble processing calcFullContext[${uid}]"))
          .asInstanceOf[U]
        val nextModel = uState.iterate(src)
        if (nextModel.isDefined)
          src
            .withContext(src.context.updated(uid, nextModel.get.withContext(src.context)))
            .asInstanceOf[dataset[A with U]]
        else
          throw new Error(s"trouble processing calcFullContext[${uid}]")
      }
    }
    /*
      Takes arguments model U with dependency set A, and dataset B wher B is U's output type. Returns a dataset[A with B].
      This method closes the gap between the functionality of calc and map, where calcFor can be used to update or expand a dataset
      to have a new value for B where B may or may not be equal to U.
     */
//    def calcFor[U <: model[A, B], B <: dataset[_]](
//      implicit tag: TypeTag[U],
//      tagb: TypeTag[B],
//      atag: TypeTag[A]
//    ): dataset[A with B] = {
//      val uid = buildId[U]
//      val instB = build[B]
//      val bid = buildId[B]
//      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(buildId[A]))
//      else {
//        val next = instU.iterate(src)
//        if (next.isDefined)
//          src
//            .withContext(src.context.updated(bid, next.get))
//            .asInstanceOf[dataset[A with B]]
//        else throw new Error(s"Error while processing calcFor[${uid},${bid}]")
//      }
//    }
    /*
      Takes arguments U where U is a model with dependency set A and output type U, that has a guarenteed value
      in its output of type dataset[_>:A], and returns D:dataset[A]  where all values for types in D are either updated
      by U's iteration function, or carried over from the src dataset[A]

      ex: src:dataset[A with B with C]
          prog:model[A with B,prog] with TerminalType[dataset[A with B]] (i.e. prog's iterator is of type
                                                                          dataset[A with B] => (prog with value: dataset[A with B])
                                                                          )
          src.flatmap[prog] is then of type dataset[A with B with C] where A and B were updated by prog,
                                                                      and C's value is unchanged from src
     */
    def flatMap[U <: model[A, U] with produces[_ >: dataset[A]<:dataset[_]]](
      implicit taga: TypeTag[A],
      tagu: TypeTag[U]
    ): dataset[A] = {

      val uid = buildId[U]
      val instU = //build[U]
    src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it in flatMap"))
      .asInstanceOf[U]
      val nextDataset = instU.iterate(src)
      if (nextDataset.isDefined) {
        src.withContext(src.context ++ nextDataset.get.value.context)
      } else
        throw new Error(s"Error while processing flatMap[${uid}]")
    }
    def flatMapT[U <: model[A, U] with produces[_ >: dataset[A] <: dataset[_]]](
      implicit taga: TypeTag[A],
      tagu: TypeTag[U]
    ): dataset[A with FlatMap[A, U]] = {
      val uid = buildId[U]
      val instU = //build[U]
        src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it as a mapping"))
          .asInstanceOf[U]
      val nextDataset = instU.iterate(src)
      if (nextDataset.isDefined) {
        val f = FlatMap.apply[A, U](src, nextDataset.get)
        f.withContext(src.context ++ nextDataset.get.value.context)
      } else
        throw new Error(s"Error while processing flatMap[${uid}]")
    }
  }
  implicit class Fetcher[A <: dataset[_]](a: dataset[A]) {
    def fetchAs[U >: A <: dataset[_] with produces[tpe], tpe](
      implicit ttag: TypeTag[U],
      atag: TypeTag[A]
    ): Option[tpe] =
      if (a.context.isEmpty) throw new Error(contextErrorStringFetch(buildId[A]))
      else
        a.context.get(buildId[U]) match {
          case Some(d: U) => Some(d.value)
          case _          => None
        }
    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], atag: TypeTag[A]): Option[U] =
      if (a.context.isEmpty) throw new Error(contextErrorStringFetch(buildId[A]))
      else {
        val uid = buildId[U]
        (a.context.get(uid) match {
          case Some(d: U) if d.withContext(a.context) == null => Some(d.asInstanceOf[U])
          case Some(d: U)                                     => Some(d.withContext(a.context))
          case _                                              => None
        }).asInstanceOf[Option[U]]
      }
  }
  implicit class ContextBuilder(m: Map[idtype, dataset[_]]) {
    def register[U <: dataset[_]](value: U)(implicit ttag: TypeTag[U]): contexttype =
      m.updated(buildId[U], value).asInstanceOf[contexttype]
    def remove[U <: dataset[_]](implicit ttag: TypeTag[U]): contexttype =
      m.toSeq.filterNot(p => p._1 == buildId[U]).toMap.asInstanceOf[contexttype]
  }
  implicit class ContextViewer(m: Map[idtype, dataset[_]]) {
    def valueView(): Map[idtype, Any] =
      m.toList
        .map(p => {
          val k = p._1
          val v: dataset[_] = p._2
          val prettyval =
            if (v.isInstanceOf[produces[_]]) v.asInstanceOf[produces[_]].value
            else null
          k -> prettyval
        })
        .toMap
  }
  implicit class Includer[A <: dataset[_]](a: dataset[A]) {
    def include[U <: dataset[_]](value: U)(implicit ttag: TypeTag[U]): dataset[A with U] = {
      a.withContext(a.context.updated(buildId[U], value))
        .asInstanceOf[dataset[A with U]]
    }
  }
  implicit class Merger[A <: dataset[A]](a: dataset[A]) {
    def merge[B <: dataset[_]](b: dataset[B]): Option[dataset[A with B]] = {
      val matchkeys = a.context.keys.filter(k => b.context.keys.toSeq.contains(k))
      val noContradiction = matchkeys.foldLeft(true)((acc, key) => {
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
    def induct[B <: dataset[_]]() = ???
  }
  implicit class Iterator[A <: dataset[_]](a: dataset[A]) {
    def iter[F <: dataset[A] => dataset[A]](f: F): dataset[A] = f(a)
    def transact[U >: A <: dataset[_]](f: dataset[A] => dataset[U]): dataset[U] = f(a)
    def run[F <: dataset[A] => dataset[B] with produces[tpe], B <: dataset[A], tpe](
      f: F
    ): dataset[B] = a.withContext(a.context.updated(f(a).id, f(a))).asInstanceOf[dataset[B]]
  }
}

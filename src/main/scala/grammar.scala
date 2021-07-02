package Typical.core

import scala.reflect.runtime.universe._

package object grammar {

  import dataset._
  implicit def evaluate[T](t:produces[T]):T = t.value
  implicit class evaluatorModels[A<:dataset[A] with ==>[_,A]](src:dataset[A])(implicit taga:TypeTag[A]){
    def getValue[T](implicit  ev: A <:< produces[T]):dataset[Nothing] with produces[T] =
      if(src.isInstanceOf[A]) Val(ev(src.asInstanceOf[A]).value)
    else for{
      a <- src.fetch[A]
    }yield Val(a.value)
  }
  implicit class evaluatorAxioms[A<:dataset[A] with ::[A]](src:dataset[A])(implicit taga:TypeTag[A]){
    def getValue[T](implicit  ev: A <:< produces[T]):Val[T] =
      if(src.isInstanceOf[A]) Val(ev(src.asInstanceOf[A]).value)
      else for{
        a <- src.fetch[A]
      }yield Val(a.value)
  }
  implicit class MonadicDatasets[B <: dataset[B]](m: dataset[B])(implicit tagb: TypeTag[B]) {
    def flatMap[C <: dataset[_], U](f: B => dataset[C] with U)(implicit tagC: TypeTag[C]): dataset[C] with U =
      if (m.isEmpty) {
        val lasterr = m.asInstanceOf[DatasetError[B]].value
        DatasetError[C](lasterr:_*).asInstanceOf[dataset[C] with U]
      } else
        f(m.asInstanceOf[B])

    def map[C <: dataset[_],U](f: B => dataset[C] with U)(implicit tagc: TypeTag[C]): dataset[C] with U =
      if (m.isEmpty) {
        val lasterr = m.asInstanceOf[DatasetError[B]].value
        DatasetError[C](lasterr:_*).asInstanceOf[dataset[C] with U]
      } else
        f(m.asInstanceOf[B])

  }

implicit class toOption[A<:dataset[A]](src:dataset[A]){
  def toOption:Option[A] = if(!src.isEmpty)Some(src.asInstanceOf[A]) else None
  def self:A = if(src.isEmpty) throw src.asInstanceOf[DatasetError[A]].value.head else src.asInstanceOf[A]
}

  implicit class UniversalDatasetOps[A <: dataset[_]](src: dataset[A]) {

    def console(dat:dataset[A] = src):dataset[A] = {
      val cmd = scala.io.StdIn.readLine()
      val matchingDatasets = dat.context.values.filter(_.toString.toUpperCase.contains(cmd.toUpperCase())).map(d => d match {
        case p:produces[_] =>  s"${d.toString}\n\t${p.value.toString.take(100)}"
        case _ => d.toString
      } ).foldLeft("")(_ + "\n" + _)
      val commands = Set("iter","run","derive","fetch")
      val matchingcommands = commands.filter(_.toUpperCase == cmd.toUpperCase)
      cmd match {
        case "exit" => return dat
        case _ if matchingDatasets.nonEmpty => println(s"--------------------------------------------------$matchingDatasets\n-------------------------------------------------------\n")
        //case _ if matchingcommands.nonEmpty => println(dat.)
        case _ => println("unrecognized command")

      }
      console()
    }
    //def isDefinedAt[]:Boolean
    def iter[U <: Function1[dataset[A], dataset[U]] with dataset[U]](
                                implicit ttag: TypeTag[U],
                                tagA: TypeTag[A]
                              ): dataset[A with U] = {
      if (src.isEmpty) {
        DatasetError[A with U](new Error(s"Error while doing iter ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      }
      else
        src.derive[U].fold(
          e => DatasetError[A with U](new Error(s"Failure to iter ${buildId[U]}")).append(e.value: _*)
        )(
          (nextU: dataset[U]) => src.include[U, U](nextU.asInstanceOf[U])
        )
    }

    def derive[U <: Function1[dataset[A], dataset[U]] with dataset[U]](implicit utag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if (src.isEmpty) DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      else
        src.asInstanceOf[dataset[A with U]].multifetch[U].fold(
          e => DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(e.value: _*)
        )(
          ustate => ustate.asInstanceOf[U].apply(src)
        )
    }

    def run[U <: A ==>  (_ >: dataset[A] <: dataset[_])](
                                                           implicit ttag: TypeTag[U],
                                                           tagA: TypeTag[A]
                                                         ): dataset[A] =
      if (src.isEmpty) src
      else
        src.asInstanceOf[dataset[A with U]].multifetch[U].fold(
          e => DatasetError[A](new Error(s"Failure to run ${buildId[U]}, No runtime instance")).append(e.value: _*)
        )(instU => {
            instU.asInstanceOf[U].apply(src).fold(
              e => DatasetError[A](new Error(s"Failure to run ${buildId[U]}")).append(e.value: _*)
            )(
              b => src.withContext(src.context ++ b.context)
            )
        })

  def runWith[B <: dataset[A],U <: A ==> B](instU:U with (A ==> B))(
                                                         implicit ttag: TypeTag[U],
                                                         tagA: TypeTag[A],
                                                         tagb:TypeTag[B]
                                                       ): dataset[A] =
    if (src.isEmpty) DatasetError[A]()
    else
        instU.apply(src).fold(
          e => DatasetError[A](new Error(s"Failure to run ${buildId[U]}")).append(e.value: _*)
        )(
          b => if(b.context.isEmpty) src.include(b) else src.withContext(src.context ++ b.context)
        )

  }


  implicit class Fetcher[A <: dataset[_]](src: dataset[A]) {

    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] =
      src.multifetch[U]

    /*
      This method exists primarily to allow the generalization of a good fetch method, while also not restricting
      the fetched type U to be of type dataset[U]. This is of particular interest in the run method, where we
      are usually going to need to fetch a stateful constructor for of the form dataset[A with B with C] => dataset[A with B with C]
      as one example. The non-private 'fetch' method in turn just restricts its type U to be of form dataset[U],
     */
    private[grammar] def multifetch[U >: A <: dataset[_]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if(src.isEmpty) DatasetError[U](new Error(s"Error while performing fetch[${buildId[U]}]")).append(src.asInstanceOf[DatasetError[A]].value:_*)
      else {
        val uid = buildIdLor[U](src.relations)
        src.context.get(uid) match {
          case Some(d: U) if d != null && d.isInstanceOf[U] =>
            apply[U](d)
          case _ if ttag.tpe =:= tagA.tpe => src
          case _ => DatasetError[U](new Error(s"No value for ${buildId[U]} found for fetch[$uid] in $src"))
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

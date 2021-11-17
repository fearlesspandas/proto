package Typical.core

import scala.reflect.runtime.universe._

package object grammar {

  import dataset._
  implicit def evaluate[T](t:produces[T]):T = t.value
  implicit class evaluatorModels[A<:dataset[A]](src:dataset[A])(implicit taga:TypeTag[A]){
    def getValue[T](implicit  ev: A <:< produces[T]): produces[T] =
      if(src == null) noVal(new Error("null Value found"))
      else if(src.isInstanceOf[A]) ev(src.asInstanceOf[A])
    else {
        val a = src.<--[A]
        a.biMap[produces[T]](
          err => noVal(err.value:_*)
        )(
          d => ev(d.get)
        )
      }
    def getOrElse[T](default:T)(implicit  ev: A <:< produces[T]): produces[T] = if(src == null)
    noVal(new Error(s"null value found for ${buildId[A]}"))
    else{
      src.biMap[produces[T]](err => someval(default) )(d =>  d.getValue[T])
    }
  }
//  implicit class evaluatorAxioms[A<:dataset[A] with ::[A]](src:dataset[A])(implicit taga:TypeTag[A]){
//    def getValue[T](implicit  ev: A <:< produces[T]): produces[T] =
//      if(src.isInstanceOf[A]) ev(src.asInstanceOf[A])
//      else {
//        val a = src.<--[A]
//        a.biMap[produces[T]](
//          err => noVal(err.value:_*)
//        )(
//          d => ev(d.asInstanceOf[A])
//        )
//      }
//  }
  implicit class MonadicDatasets[B <: dataset[B]](m: dataset[B])(implicit tagb: TypeTag[B]) {
    def get:B = m.asInstanceOf[B]
    def flatMap[C <: dataset[_]](f: B => dataset[C])(implicit tagC: TypeTag[C]): dataset[C] =
      if (m.isEmpty) {
        val lasterr = m.asInstanceOf[DatasetError[B]].value
        DatasetError[C](lasterr:_*).asInstanceOf[dataset[C]]
      } else
        f(m.asInstanceOf[B])

    def map[C <: dataset[_]](f: B => dataset[C] )(implicit tagc: TypeTag[C]): dataset[C] =
      if(m == null)DatasetError[C](new Error(s"null value found for ${buildId[B]}")).asInstanceOf[dataset[C]]
      else if (m.isEmpty) {
        val lasterr = m.asInstanceOf[DatasetError[B]].value
        DatasetError[C](lasterr:_*).asInstanceOf[dataset[C]]
      } else {
        val res = f(m.asInstanceOf[B])
        //if(res.isContext) res.multifetch[C] else res
        res
      }

  }

  implicit class FromOption[A<:dataset[A]](src:Option[A])(implicit taga:TypeTag[A]){
    def fromOption:dataset[A] = if(!src.isEmpty) src.get else DatasetError[A](new Error(s"None found for Option[${buildId[A]}]"))
  }
implicit class toOption[A<:dataset[A]](src:dataset[A]){
  def toOption:Option[A] = if(!src.isEmpty)Some(src.asInstanceOf[A]) else None
  def self:A = if(src.isEmpty) throw src.asInstanceOf[DatasetError[A]].value.head else src.asInstanceOf[A]
}

  implicit class UniversalDatasetOps[A <: dataset[_]](src: dataset[A]) {
    def processCtx(str:String):String = str.replaceAll("class","").replaceAll("trait","").replaceAll("object","")
    import scala.tools.reflect._
    def compile(code: String,passedcontext:contexttype)(implicit taga:TypeTag[A]): (contexttype) => A = {
      val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
      val typeString = passedcontext.values.tail.foldLeft(passedcontext.values.head.getClass.getTypeName)((acc,k) => {
        val filteredKey = k.getClass.getTypeName
        s"$filteredKey with $acc"
      })
      val tree = tb.parse(
        s"""
           |import Typical.core._
           |  import Account._
           |  import GrowAccounts._
           |  import grammar._
           |  import Property._
           |  import Date._
           |  import AccountRates._
           |  import Income._
           |def wrapper(context: Map[Any,Typical.core.dataset.dataset[_]]): Any = {
           |  val src = Typical.core.dataset.data[$typeString](context,Map())
           |  $code
           |}
           |wrapper _
      """.stripMargin)
      val f = tb.compile(tree)
      val wrapper = f()
      wrapper.asInstanceOf[contexttype => A]
    }
    def console(implicit taga:TypeTag[A]):dataset[A] = {


      println(s"Welcome to console for ${taga.tpe.toString}")
      val cmd = scala.io.StdIn.readLine()
      val matchingDatasets = src.context.values.filter(_.toString.toUpperCase.contains(cmd.toUpperCase())).map(d => d match {
        case p:produces[_] =>  s"${d.toString}\n\t${p.value.toString.take(100)}"
        case _ => d.toString
      } ).foldLeft("")(_ + "\n" + _)
      val commands = Set("iter","run","derive","fetch")
      val matchingcommands = commands.filter(_.toUpperCase == cmd.toUpperCase)
      cmd match {
        case "exit" => return src
        case _ if matchingDatasets.nonEmpty => println(s"--------------------------------------------------$matchingDatasets\n-------------------------------------------------------\n")
        //case _ if matchingcommands.nonEmpty => println(dat.)
        case s => try{compile(s,src.context)(taga)(src.context)}catch{case e => println(e.getMessage)}
        case _ => println("unrecognized command")

      }
      console
    }
    def solve[U<: ==>[A,A] with solveable[A]](implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A]  = {
      src.asInstanceOf[dataset[A with U]].multifetch[U].fold(err => err.asInstanceOf[dataset[A]])(
        uState => {
          val ufunc = uState.asInstanceOf[U]
          if (ufunc.solved(src)) {
            return src
          }
          val thisSolution = src.-->[U]
          if(ufunc.solved(thisSolution))
            return thisSolution
          else{
            val nextData = ufunc.next(src).map({case a :dataset[A] => a;case _ => DatasetError[A](new Error("context not applicable"))})
            if(nextData.size == 0)
              DatasetError[A](new Error("No Solution found"))
            else{
              val nextSolutionSet =
                nextData
                .filterNot(_.isEmpty)
                  .map(_.solve[U])
                  .collectFirst({case d if (!d.isEmpty) => d})

              nextSolutionSet match {
                case Some(d:dataset[A]) => d
                case _ => DatasetError[A](new Error("No solution found"))
              }
            }
          }
        }
      )
    }

    //def isDefinedAt[]:Boolean
    def +->[U <: Function1[dataset[A], dataset[U]] with dataset[U]](
                                implicit ttag: TypeTag[U],
                                tagA: TypeTag[A]
                              ): dataset[A with U] = {
      if (src.isEmpty) {
        DatasetError[A with U](new Error(s"Error while doing iter ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      }
      else
        src.<-+[U].fold(
          e => DatasetError[A with U](new Error(s"Failure to iter ${buildId[U]}")).append(e.value: _*)
        )(
          (nextU: dataset[U]) => src.++[U, U](nextU.asInstanceOf[U])
        )
    }

    def <-+[U <: Function1[dataset[A], dataset[U]] with dataset[U]](implicit utag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if (src.isEmpty) DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(src.asInstanceOf[DatasetError[A]].value: _*)
      else
        src.asInstanceOf[dataset[A with U]].multifetch[U].fold(
          e => DatasetError[U](new Error(s"Error while doing derive ${buildId[U]}")).append(e.value: _*)
        )(
          ustate => ustate.asInstanceOf[U].apply(src)
        )
    }

    def -->[U <: A ==>  (_ >: dataset[A] <: dataset[_])](
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
              b => src.withContext(src.context ++ b.context).withRelations(src.relations ++ b.relations)
            )
        })

  def -->[B<:dataset[_]](instU:A ==> B)(
                                                         implicit
                                                         tagA: TypeTag[A],
                                                         tagb:TypeTag[B]
                                                       ): dataset[B] =
    if (src.isEmpty) DatasetError[B](src.asInstanceOf[DatasetError[A]].value:_*)
    else
        instU.apply(src).fold(
          e => DatasetError[B](new Error(s"Failure to run ${instU.toString}")).append(e.value: _*)
        )(
          b => b
        )

  }


  implicit class Fetcher[A <: dataset[_]](src: dataset[A]) {
    require(src.isContext,s"${src.toString} is not a valid context")
    def <--[U >: A <: dataset[U]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] =
      src.multifetch[U]


    /*
      This method exists primarily to allow the generalization of a good fetch method, while also not restricting
      the fetched type U to be of type dataset[U]. This is of particular interest in the run method, where we
      are usually going to need to fetch a stateful constructor for of the form dataset[A with B with C] => dataset[A with B with C]
      as one example. The non-private 'fetch' method in turn just restricts its type U to be of form dataset[U],
     */
     def multifetch[U >: A <: dataset[_]](implicit ttag: TypeTag[U], tagA: TypeTag[A]): dataset[U] = {
      if(src.isEmpty) DatasetError[U](new Error(s"Error while performing fetch[${buildId[U]}]")).append(src.asInstanceOf[DatasetError[A]].value:_*)
      else {
        val uid = buildIdLor[U](src.relations)
        src.context.get(uid) match {
          case Some(d: U) if d != null && d.isInstanceOf[U] =>
            apply[U](d) //+-[U] d
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
    def ++[U <: dataset[_], T <: U](value: T)(implicit taga:TypeTag[A], ttag: TypeTag[U], tag: TypeTag[T]): dataset[A with U with T] = {
      if (value == null || value.isEmpty) DatasetError[A with U with T](new Error(s"value is empty for ${buildId[U]}")).append(
        (
          if (value != null)value.asInstanceOf[DatasetError[T]].value.toSeq
          else Seq.empty[Error]
        ):_*)
//      else if(!a.isContext)
//        data[A with U with T]() ++ a ++ value
      else {
        val newRelations = a.relations.updated(buildIdLor[U](a.relations), buildIdLor[T](a.relations))
        val newContext = a.context.updated(buildIdLor[U](newRelations), value)
        a.withRelations(newRelations).withContext(newContext)
          .asInstanceOf[dataset[A with U with T]]
      }

    }
    def +-[U<:dataset[_]](value:U)(implicit tagu:TypeTag[U],taga:TypeTag[A]):dataset[A with U] = {
      if(a.context.contains(buildIdLor[U](a.relations)))
        a.asInstanceOf[dataset[A with U]]
      else a.++(value)
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

package Grammar
import Grammar.Calc
import Grammar.FlatMap
import Grammar.TMap
import Typical.core.grammar.Container

import scala.reflect.runtime.universe._
trait GrammarBase[C[_,_]<:Either[_,_]] {
  import dataset._
  type thing[A[_]<:Option[_]]
  type Container[+A] = Either[Error,A]
  implicit class CalcerOp[A<:dataset[_]](src:Container[dataset[A]]){
    def calc[U<:model[A,U]](implicit ttag:TypeTag[U]):Container[dataset[A with U]] = {
      src.flatMap(x => x.calc[U])
    }
    //def calcT[U <: model[A, U]](implicit ttag: TypeTag[U], atag: TypeTag[A]): Container[dataset[A with U with Calc[_, U]]] = src.flatMap(_.calcT[U])
    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], atag: TypeTag[A]): Container[U] = src.flatMap(_.fetch[U])
    def derive[U <: model[A, U]](implicit utag:TypeTag[U],atag:TypeTag[A]):Container[U] = src.flatMap(_.derive[U])
    def flatCalc[U <: model[A, U] with produces[_ >: dataset[A]<:dataset[_]]](
                                                                               implicit taga: TypeTag[A],
                                                                               tagu: TypeTag[U]
                                                                             ): Container[dataset[A]] = src.flatMap(_.flatCalc[U])
//    def flatMapT[U <: model[A, U] with produces[_ >: dataset[A] <: dataset[_]]](
//                                                                                 implicit taga: TypeTag[A],
//                                                                                 tagu: TypeTag[U]
//                                                                               ): Container[dataset[A with FlatMap[A, U]]] = src.flatMap(_.flatMapT[U])

  }
  implicit class Calcer[A <: dataset[_]](src: dataset[A]) {
    /*
      Takes a model U with dependency set A that produces a U. Returns a (possibly expanded) dataset with both A and U
      ex, A,B,C are datasets, X is a model[A with B with C,X], and dataset[A with B with C].calc[X] will return
      a dataset[A with B with C with X]. If X is either A,B or C then an updated dataset of type dataset[A with B with C]
      will be returned
     */
    def calc2[U <: model[A, U]](
                                implicit ttag: TypeTag[U]
                              ): dataset[A with U] = {
      val uid = buildId[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(src.id))
      else (for {
        //if no U we want error
        uState <- src.calc[U]
        //uu <- uState.fetch[U]
      }yield (uState)).flatten
    }
    def calc[U <: model[A, U]](
                                implicit ttag: TypeTag[U]
                              ): dataset[A with U] = {
      val uid = buildId[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(src.id))
      else  {
        //if no U we want error
        val uState = //validateResult(src)(
          src.context
            .get(uid)
            .map { case u: U if u != null => u}
          .get
       // )
        val nextU = uState.iterate(src)
        src
          .withContext(src.context.updated(uid, nextU))
          .asInstanceOf[dataset[A with U]]
      }

    }


    def derive[U <: model[A, U]](implicit utag:TypeTag[U],taga:TypeTag[A]):dataset[U] = for {
      res <- src.calc[U]
    }yield res.fetch[U]

    /*
      Takes a model U with dependency set A, and returns a dataset[A] with an updated value for the output
      of U. This method is similar to calc except in the following behavior:
        1.calc allows for the ability to expand a dataset[A] to a dataset[A with B], whereas map will always
          return a dataset[A]
        2.map allows for the model U to have an output that is not of type U, whereas calc requires that U models type U.
          i.e. U can be of type model[A with B, C] where C != U, and calling dataset[A with B with C].map[U] will return
          a dataset[A with B with C] with an updated value for C.
     */
    def mapx[U <: model[A , _ >: dataset[A] <: dataset[_]]](
                                                            implicit ttag: TypeTag[U]
                                                          ): Container[dataset[A]] = {
      val uid = buildId[U]
      val instU = src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it as a mapping"))
        .asInstanceOf[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(src.id))
      else for {
        b <- instU.iterate(src)//.toRight(null)
      }yield src.withContext(src.context.updated(b.id, b))


    }



    def flatCalc[U <: model[A, U] with produces[_ >: dataset[A]<:dataset[_]]](
                                                                               implicit taga: TypeTag[A],
                                                                               tagu: TypeTag[U]
                                                                             ): Container[dataset[A]] = {
      val uid = buildId[U]
      for{
        insU <- validateResult(src)(src.context.get(uid).map({case u:U if u != null => u}).toRight(null))
        nextDataset <- validateResult(src)(insU.iterate(src))
      }yield src.withContext(src.context ++ nextDataset.value.context)
    }


  }
  implicit class Fetcher[A <: dataset[_]](a: dataset[A]) {
    def fetchAs[U >: A <: dataset[_] with produces[tpe], tpe](
                                                               implicit ttag: TypeTag[U],
                                                               atag: TypeTag[A]
                                                             ): Container[tpe] =
      if (a.context.isEmpty) throw new Error(contextErrorStringFetch(buildId[A]))
      else
        a.context.get(buildId[U]) match {
          case Some(d: U) => Right(d.value)
          case _          => Left(null)
        }
    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U], atag: TypeTag[A]): U =
      if (a.context.isEmpty) throw new Error(contextErrorStringFetch(buildId[A]))
      else {
        val uid = buildId[U]
          (a.context.get(uid) match {
            case Some(d: U) if d.withContext(a.context) == null => Some(d.asInstanceOf[U])
            case Some(d: U) => Some(d.withContext(a.context))
            case _ => None
          }).asInstanceOf[U]

      }
    def fetchId[U >: A <: dataset[U]](id:Id[_>:A<:dataset[_]])(implicit ttag: TypeTag[U], atag: TypeTag[A]): Container[U] =
      if (a.context.isEmpty) throw new Error(contextErrorStringFetch(buildId[A]))
      else {
        val uid = id.dat.id
        validateResult(a)(a.context.get(uid).asInstanceOf[Container[U]])
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
    def includeOp[U <: dataset[_]](value: Container[U])(implicit ttag: TypeTag[U]): dataset[A with U] = value match {
      case Right(u) => a.withContext(a.context.updated(buildId[U], u))
        .asInstanceOf[dataset[A with U]]
      case Left(_) => a.withContext(a.context.filter(p => p._1 != buildId[U])).asInstanceOf[dataset[A with U]]
    }
  }
  implicit class Merger[A <: dataset[A]](a: dataset[A]) {
    def merge[B <: dataset[_]](b: dataset[B]): Container[dataset[A with B]] = {
      val matchkeys = a.context.keys.filter(k => b.context.keys.toSeq.contains(k))
      val noContradiction = matchkeys.foldLeft(true)((acc, key) => {
        acc && b.context.get(key) == a.context.get(key)
      })
      noContradiction match {
        case false => Left(null)
        case true =>
          Right(
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


  object dataset {
    //  def getTypeId[A] = {
    //    lazy val res = typeTag[A].tpe
    //    res
    //  }

    def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

    def m[T: TypeTag, S: TypeTag](x: T, y: S): Boolean = {
      val leftTag = typeTag[T]
      val rightTag = typeTag[S]
      leftTag.tpe <:< rightTag.tpe
    }

    def buildId[A: TypeTag]: idtype = typeTag[A].tpe.typeSymbol.toString()

    // can build an instance that from type info alone that does not take arguments
     def build[A: TypeTag]: A = {
      val m = runtimeMirror(getClass.getClassLoader)
      val classThing = typeOf[A].typeSymbol.asClass
      val cm = m.reflectClass(classThing)
      val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
      //val test = c1.info.resultType.paramLists
      val c2 = cm.reflectConstructor(c1)
      c2.apply().asInstanceOf[A]
    }

     def buildInferred[A](implicit create: (TypeTag[A]) => A, a: TypeTag[A]): A = {
      create(a)
    }

    def validateResult[A <: dataset[_]](src: dataset[_])(dat: Container[A])(implicit taga: TypeTag[A]): Container[A] = dat match {
      case Right(res) => Right(res)
      case _ =>
        throw new Error(s"No value for ${buildId[A]} found")

    }

    def contextErrorStringFetch(className: idtype) =
      s"""
  Using type ${className} for fetch context is not allowed. To allow it either,
  (1) override the 'withContext' method in ${className},and define it to update the context val for ${className} types
   or
  (2) override it's context val to be a static value.
  """

    def contextErrorStringCalc(className: idtype) =
      s"""
  Using type ${className} for calc context is not allowed.
   To allow it override the 'withContext' method in ${className},
   and define it to update the context val for the ${className} type
  """

    type contexttype = Map[idtype, dataset[_]]
    type idtype = Any


    trait produces[+T] {
      val value: T // = null.asInstanceOf[T]
    }

    trait failsWith[+T] {
      val err: T
    }

    trait Id[+A <: dataset[_]] {
      val dat: A
    }

    trait InitialType[-T <: dataset[_]] {
      val terminalValue: dataset[_ >: T <: dataset[_]] = null
    }
    def apply[A<:dataset[_]](a:A):dataset[A] = a.asInstanceOf[dataset[A]]
//    sealed trait dataset[+A<:dataset[_]]{
//      //def flatMap[B<:dataset[_]](f:dataset[A] => dataset[B]):dataset[B] = f(this)
//      def iter[dep<:dataset[_]](src:dataset[dep]):dataset[A] = null
//      def flatMap[B<:dataset[_]]:dataset[B with A] = null
//      def map[B<:dataset[_]](f:dataset[A] => B):dataset[B] = apply[B](f(this))
//    }
//    case class Res[A<:dataset[_]](value:A) extends dataset[A]
//    case object NoRes extends dataset[Nothing]

    trait dataset[+A <: dataset[_]] {
      def flatMap[B<:dataset[_]](f:dataset[A] => dataset[B]):dataset[B] = f(this)
      def map[B<:dataset[_]](f:dataset[A] => B):dataset[B] = apply[B](f(this))
      def flatten[B](implicit ev: A<:< dataset[B]):dataset[B] = this.asInstanceOf[dataset[B]]
      val context: contexttype
      //val errorMap:Map[idtype,Error]
      val IdRelations: Map[idtype, idtype]
      def flatMap[B<:dataset[_]](f:dataset[A] => dataset[B]): dataset[B] = f(this)
      def withContext(ctx: contexttype): dataset[A]

      //def withError()
      val id: idtype
    }

    trait axiom[T, A <: axiom[T, A]] extends dataset[A] with produces[T] {
      override final val id = buildId[this.type]
      override val context: contexttype = Map()
      //override val errorMap: Map[idtype, Error] = Map()
      override val IdRelations: Map[idtype, idtype] = Map(id -> id)

      override def withContext(ctx: contexttype): dataset[A] = null

      def withValue(newVal: T): A
    }

    trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] {
      self =>
      def iterate(src: dataset[dependencies]): dataset[output]

      //def flatMap[B <: dataset[_]](src:dataset[dependencies]): dataset[B with output]
      override final val id = buildId[this.type]
      //override val errorMap: Map[idtype, Error] = Map()
      override val context: contexttype = Map()

      override def withContext(ctx: contexttype): dataset[output] = null
    }

    trait FinalModel[-dependencies <: dataset[_], +output <: dataset[_]] extends model[dependencies, output] {
      override val IdRelations: Map[idtype, idtype] = Map(id -> id)
    }

    trait Mmodel[dep <: dataset[_], A <: dataset[_], T] extends model[dep, A] with produces[T] {
      val f: dataset[dep] => (T, A)
    }


    trait modelUnion[dependencies <: dataset[_], +self <: modelUnion[_, self]] extends FinalModel[dependencies, self] with produces[dataset[dependencies]] {
      def next(src: dataset[dependencies]): Container[dataset[dependencies]]

      def apply(value: dataset[dependencies]): self

      final override def iterate(src: dataset[dependencies]): Container[self] = for {
        d <- next(src)
      } yield apply(d)
    }

    case class data[A <: dataset[_]](override val context: contexttype) extends dataset[A] {
      override def withContext(ctx: contexttype): dataset[A] = {
        // val currErrors = this.errorMap
        new data[A](ctx)
      }

      override val id: idtype = null.asInstanceOf[idtype]
      override val IdRelations: Map[idtype, idtype] = context.values.foldLeft(Map[idtype, idtype]())((a, c) => a ++ c.IdRelations)

      def dataset = this.asInstanceOf[dataset[A]]

      //override val errorMap: Map[idtype, Error] = context.values.map{case d:failsWith[_] => d.id -> d.err; case d => d.id -> new Error(s"Failed to locate ${d.id}")}.toMap
    }

  }

}

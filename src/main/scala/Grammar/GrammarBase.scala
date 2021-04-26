package Grammar

import scala.reflect.runtime.universe._
object GrammarBase{
  import dataset._

  implicit class FFFF[B<:model[_<:dataset[_],B]](m:dataset[B]){
    def flatMap[C<:dataset[_]](f:B=> dataset[C]):dataset[C] = if (m.isEmpty){
      throw m.asInstanceOf[DatasetError[B]].value
    } else f(m.asInstanceOf[B])

    def map[C<:dataset[C]](f:B => dataset[C]):dataset[C] = if (m.isEmpty) throw m.asInstanceOf[DatasetError[B]].value else f(m.asInstanceOf[B])
  }
  implicit class FFFFF[A<:dataset[_],B<:dataset[_]](m:model[A,B]){
    def flatMap[C<:dataset[_]](f:dataset[B]=> dataset[C]):dataset[C] = if (m.isEmpty){
      //val r = f(this)
      //DatasetError[B](this.asInstanceOf[DatasetError[A]].value,r.id)
      throw m.asInstanceOf[DatasetError[B]].value
    } else f(m)
    def map[C<:dataset[_]](f:dataset[B] => dataset[C]):dataset[C] = if (m.isEmpty) throw this.asInstanceOf[DatasetError[B]].value else f(m)
  }
  implicit class Calcer[A <: dataset[_]](src: dataset[A]) {

    def calc[U <: model[A, U]](
                                implicit ttag: TypeTag[U]
                              ): dataset[A with U] = {
      if (src.withContext(Map()) == null) throw new Error(s"override ${src.id}'s withContext method to use it as a data context'")
      else {
        val id = buildId[U]
        val res = src.derive[U].flatMap(nextU => src.include[U](nextU))
        res
      }
    }

    //def flatCalc[U<:model[A,U] with produces[dataset[A]]]

    def derive[U <: model[A, U]](implicit utag:TypeTag[U]):dataset[U] = for {
       ustate <- src.asInstanceOf[dataset[A with U]].fetch[U]//.asInstanceOf[U]
      res <- ustate.iterate(src)//.asInstanceOf[U]
    }yield res


    def run[U <: model[A , _ >: dataset[A] <: dataset[_]]](
                                                            implicit ttag: TypeTag[U]
                                                          ): dataset[A] = {
      val uid = buildId[U]
      val instU = src.context.get(uid).getOrElse(throw new Error(s"register $uid to use it as a mapping"))
        .asInstanceOf[U]
      if (src.withContext(Map()) == null) throw new Error(contextErrorStringCalc(src.id))
      else ( {
        val b = instU.iterate(src)//.toRight(null)
        src.withContext(src.context ++ b.context)
      } )//.flatten


    }


  }
  implicit class Fetcher[A <: dataset[_]](a: dataset[A]) {

    def fetch[U >: A <: dataset[U]](implicit ttag: TypeTag[U]): dataset[U] =
      if (a.isEmpty) throw a.asInstanceOf[DatasetError[A]].value
      else {
        val uid = buildId[U]
          a.context.get(uid) match {
            case Some(d: U) if d != null => d
            case _ => DatasetError[U](new Error(s"No value for ${uid} found for fetch[$uid]"),uid)
          }

      }
  }
  implicit class ContextBuilder(m: Map[idtype, dataset[_]]) {
    def register[U <: dataset[_]](value: U)(implicit ttag: TypeTag[U]): contexttype = {
      val id = buildId[U]
      m.updated(buildId[U], value).asInstanceOf[contexttype]
    }

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
  implicit class Iterator[A <: dataset[_]](a: dataset[A]) {
    def iter[F <: dataset[A] => dataset[A]](f: F): dataset[A] = f(a)
    def transact[U >: A <: dataset[_]](f: dataset[A] => dataset[U]): dataset[U] = f(a)
    def run[F <: dataset[A] => dataset[B] with produces[tpe], B <: dataset[A], tpe](
                                                                                     f: F
                                                                                   ): dataset[B] = a.withContext(a.context.updated(f(a).id, f(a))).asInstanceOf[dataset[B]]
  }


  object dataset {


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

    trait failsWith[+T<:Error]{
      val err: T
    }

    trait Id[+A <: dataset[_]] {
      val dat: A
    }

    trait InitialType[-T <: dataset[_]] {
      val terminalValue: dataset[_ >: T <: dataset[_]] = null
    }
    def apply[A<:dataset[_]](a:A):dataset[A] = a.asInstanceOf[dataset[A]]

    trait dataset[+A <: dataset[_]] {
      def isEmpty:Boolean
//      def flatMap[B<:dataset[_]](f:A=> dataset[B]):dataset[B] = if (isEmpty){
//        //val r = f(this)
//        //DatasetError[B](this.asInstanceOf[DatasetError[A]].value,r.id)
//        throw this.asInstanceOf[DatasetError[A]].value
//      } else this match {
//        case err:DatasetError[_] => throw err.value
//        case a:axiom[_,A] => f(a.get)
//        //case m:model[_,A] => f(m.iterate())
//
//      }
      //def map[B<:dataset[_]](f:A => B):dataset[B] = if (isEmpty) throw this.asInstanceOf[DatasetError[A]].value else apply[B](f(this.asInstanceOf[A]))
      def flatten[B<:dataset[_]](implicit ev: A<:< dataset[B]):dataset[B] = if (isEmpty) throw this.asInstanceOf[DatasetError[A]].value else this.asInstanceOf[dataset[B]]
      val context: contexttype
      def withContext(ctx: contexttype): dataset[A]
      //val get:A
      val id: idtype
    }

    trait axiom[T, A <: axiom[T, A]] extends dataset[A] with produces[T] {
      override final val id = buildId[this.type]
      override val context: contexttype = Map()
      override final def isEmpty = false
      override def withContext(ctx: contexttype): dataset[A] = null
      val get:A = this.asInstanceOf[A]
      def withValue(newVal: T): A
    }

    trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] {
      self =>
      def iterate(src: dataset[dependencies]): dataset[output]

      //def flatMap[B <: dataset[_]](src:dataset[dependencies]): dataset[B with output]
      override final val id = if(this.isInstanceOf[model[_,self.type]]) buildId[self.type] else "null"

      override final def isEmpty: Boolean = false
      //override val errorMap: Map[idtype, Error] = Map()
      override val context: contexttype = Map()
     //val get:output = if (this.isInstanceOf[model[_,this.type]]) this.asInstanceOf[output] else throw new Error("")
      override def withContext(ctx: contexttype): dataset[output] = null
    }


    case class DatasetError[+A<:dataset[_]](value:Error,par_id:idtype) extends dataset[A] with produces[Error] {
      override val context:contexttype = Map()
      override final def isEmpty: Boolean = true
      override def withContext(ctx: contexttype): dataset[A ] = new DatasetError[A](this.value,this.par_id){
        override val context = ctx
      }
      //override final val get:A = throw new Error("")
      override val id: idtype = par_id
    }


    case class data[A <: dataset[_]](override val context: contexttype) extends dataset[A] {
      override def withContext(ctx: contexttype): dataset[A] = {
        new data[A](ctx)
      }
      //override final val get:A = throw new Error("")
      override final def isEmpty: Boolean = false
      override val id: idtype = null.asInstanceOf[idtype]
      def dataset = this.asInstanceOf[dataset[A]]
    }

  }

  case class MyCounter(value:Int) extends model[MyCounter,MyCounter] with produces[Int] with failsWith [Error]{
    override def iterate(src: dataset.dataset[MyCounter]): dataset[MyCounter] = for {
      currcount <- src.fetch[MyCounter]
    }yield MyCounter(currcount.asInstanceOf[MyCounter].value + 1)

  override val err: Error = new Error("No thing found")
}

  case class UpdateCounter() extends model[MyCounter,MyCounter] {
    override def iterate(src: dataset[MyCounter]): dataset[MyCounter] = src.include[MyCounter](MyCounter(1000))
  }
  def main(args:Array[String]):Unit = {
    val dat = data[MyCounter with UpdateCounter](Map[Any,dataset[_]]()
      .register[MyCounter](MyCounter(0))
        .register[UpdateCounter](UpdateCounter())
    )
    println(dat.calc[MyCounter].calc[MyCounter].calc[MyCounter].run[UpdateCounter].context)
  }
}

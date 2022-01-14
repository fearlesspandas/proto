package Typical.core;


import scala.reflect.runtime.universe.{TypeTag,typeTag,runtimeMirror,typeOf,termNames}
import grammar._
package object dataset {
  private[core] type contexttype = Map[idtype, dataset[_]]

  private[core] type idtype = Any


  def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

  def compareTypes[T: TypeTag, S: TypeTag](x: T, y: S): Boolean = {
    val leftTag = typeTag[T]
    val rightTag = typeTag[S]
    leftTag.tpe <:< rightTag.tpe
  }

  private[core] def buildId[A: TypeTag]: idtype = typeTag[A].tpe.typeSymbol.toString()

  private[core] def buildIdLor[A: TypeTag](rel:Map[idtype,idtype], next:Option[idtype] = None,curr:Option[idtype] = None): idtype = next match {
    case _ if rel.isEmpty => buildId[A]
    case Some(n) if curr.exists(_ == n) => n
    case Some(n) => buildIdLor[A](rel,rel.get(n),Some(n))
    case None => curr match {
      case Some(c) => c
      case None =>
        val base = buildId[A]
        buildIdLor(rel,rel.get(base),Some(base))
    }
  }

  // can build an instance that from type info alone that does not take arguments
  private[core] def build[A: TypeTag]: A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    val c2 = cm.reflectConstructor(c1)
    c2.apply().asInstanceOf[A]
  }


  private[core] def apply[A<:dataset[_]](a:A):dataset[A] = a.asInstanceOf[dataset[A]]


  trait produces[+T]{
   val value:T
    val exists:Boolean = true
  }

  case class someval[T](value:T) extends produces[T]

  case class noVal(errors:Error *) extends produces[Nothing]{
    override val value: Nothing = null.asInstanceOf[Nothing]
    override val exists = false
  }

  sealed trait dataset[+A <: dataset[_]] {
    val isSelf = this.isInstanceOf[A]
    private[core] val context: contexttype
    private[core] val relations:Map[idtype,idtype]
    val isContext = false
    def isEmpty:Boolean
    def biMap[B](ifEmpty: DatasetError[A] => B)(f: dataset[A] => B): B = if (isEmpty) ifEmpty(this.asInstanceOf[DatasetError[A]]) else f(this)
    def fold[B<:dataset[_]](ifEmpty: DatasetError[A] => dataset[B])(f: dataset[A] => dataset[B]): dataset[B] = if (isEmpty) ifEmpty(this.asInstanceOf[DatasetError[A]]) else f(this)
    private[core] def withContext(ctx: contexttype): dataset[A]
    private[core] def withRelations(rel:Map[idtype,idtype]):dataset[A]
  }

  trait ::[+A <: ::[A]] extends dataset[A] {
    private[core] override val context: contexttype = Map()
    override final def isEmpty = false
    private[core] override def withContext(ctx: contexttype): dataset[A] = DatasetError[A](new Error(s"No withContext method available for ${this.toString}"))
    private[core] override val relations = Map()
    private[core] override def withRelations(rel:Map[idtype,idtype]):dataset[A] = DatasetError[A](new Error(s"No withRelations method available for ${this.toString}"))
  }
  type axiom[A<: ::[A]] = ::[A]

  trait ==>[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] with Function[dataset[dependencies],dataset[output]] {
    self =>
    override def toString = this.getClass.getTypeName
    override final def isEmpty: Boolean = false
    private[core] override val context: contexttype = Map()
    private[core] override def withContext(ctx: contexttype): dataset[output] =
      DatasetError[output](new Error(s"No withContext method available for ${this.toString}"))
    private[core] override val relations:Map[idtype,idtype] = Map()
    private[core] override def withRelations(rel:Map[idtype,idtype]):dataset[output] =
      DatasetError[output](new Error(s"No withRelation method available ${this.toString}"))
  }
  type model[-dep<:dataset[_],+out<:dataset[_]] = ==>[dep,out]

  trait index[ self <: dataset[_]] extends (self ==> self) {
    def apply():dataset[self]
    override def apply(src:dataset[self]):dataset[self] = apply()
  }
  trait C[X<:dataset[_],Y<:dataset[_],A<:(X ==> Y)] extends dataset[Y with C[X,Y,A]]
  case class Val[T](value:T) extends ::[Val[_]] with produces[T]

  case class DatasetError[+A<:dataset[_]](value:Error*) extends dataset[A] {
    private[core] override val context:contexttype = Map()
    private[core] override val relations: Map[idtype, idtype] = Map()
    override final def isEmpty: Boolean = true
    private[core] override def withContext(ctx: contexttype): dataset[A ] = new DatasetError[A](this.value:_*){
      override val context = ctx
    }
    private[core] def append(newvalue:Error *):DatasetError[A] = new DatasetError[A]( newvalue ++ this.value :_*  )
    private[core] override def withRelations(rel: Map[idtype, idtype]): dataset[A] = new  DatasetError[A](this.value:_*){
      override val relations = rel
    }
  }

  case class data[A <: dataset[_]](override val context: contexttype = Map(),relations:Map[idtype,idtype] = Map()) extends dataset[A] with produces [contexttype]{
    override val value = context
    private[core] override def withContext(ctx: contexttype): dataset[A] = {
      val relationsHasErr = ctx.values.exists(_.isInstanceOf[DatasetError[_]])
      if(relationsHasErr) {
        DatasetError[A]()
      }
      else
        new data[A](ctx,this.relations)
    }
    override final def isEmpty: Boolean = false
    override val isContext = true
    private[core] override def withRelations(rel: Map[idtype, idtype]): dataset[A] = data(this.context,rel)
  }

  trait solveable[A<:dataset[_]]{
    def solved(src:dataset[A]):Boolean
    def next(src:dataset[A]):Seq[dataset[A]]
  }
}

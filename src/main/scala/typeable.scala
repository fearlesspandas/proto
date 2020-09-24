package Typical.core;

import scala.reflect.runtime.universe._
package object typeable {
  def getTypeTag[T: TypeTag](obj: T) = typeTag[T]
  def m[T: TypeTag, S: TypeTag](x: T, y: S): Boolean = {
       val leftTag = typeTag[T]
       val rightTag = typeTag[S]
       leftTag.tpe <:< rightTag.tpe
     }
  private[core] def build[A:TypeTag]:A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    val test = c1.info.resultType.paramLists
    val c2 = cm.reflectConstructor(c1)
    c2.apply().asInstanceOf[A]
  }

  def contextErrorStringFetch(className:idtype) = s"""
  Using type ${className} for fetch context is not allowed. To allow it either,
  (1) override the 'withContext' method in ${className},and define it to update the context val for ${className} types
   or
  (2) override it's context val to be a static value.
  """
  def contextErrorStringCalc(className:idtype) = s"""
  Using type ${className} for calc context is not allowed.
   To allow it override the 'withContext' method in ${className},
   and define it to update the context val for the ${className} type
  """
  type contexttype = Map[Any,dataset[_]]
  type idtype = String


  trait InitialType[+T]{
    val value:T = null.asInstanceOf[T]
  }

  trait dataset[+A <: dataset[_]] extends InitialType[Any]{
    val context:contexttype
    def withContext(ctx:contexttype):dataset[A]
    def id:idtype
  }

  trait axiom[A <: axiom[A]] extends dataset[A] with InitialType[Any]{
    override final val id = this.getClass.getTypeName
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[A] = null
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] with InitialType[Any] {
    def iterate(src:dataset[dependencies]):output
    override final val id = this.getClass.getTypeName
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[output] = null
  }

  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = null
    def dataset = this.asInstanceOf[dataset[A]]
  }

}
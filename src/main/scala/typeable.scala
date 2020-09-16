package Typical.core;

import scala.reflect.runtime.universe._
package object typeable {

  private[core] def build[A:TypeTag]:A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    val c2 = cm.reflectConstructor(c1)
    c2.apply().asInstanceOf[A]
  }

  def contextErrorString(className:idtype) = s"Using type ${className} as a context is not allowed. To allow it, override the 'withContext' method in ${className}, and define it to update the context val for ${className} types"
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
    override final val id = this.getClass.getSimpleName
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[A] = null
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] with InitialType[Any] {
    def iterate(src:dataset[dependencies]):output
    override final val id = this.getClass.getSimpleName
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[output] = null
  }

  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = null
    def dataset = this.asInstanceOf[dataset[A]]
  }


}
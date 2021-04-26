package Typical.core;

import Typical.core.Id._

import scala.reflect.runtime.universe._
import grammar._
package object dataset {


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


//  trait produces[+T] {
//    val value: T // = null.asInstanceOf[T]
//  }

  trait Id[+A <: dataset[_]] {
    val dat: A
  }

  trait InitialType[-T <: dataset[_]] {
    val terminalValue: dataset[_ >: T <: dataset[_]] = null
  }
  def apply[A<:dataset[_]](a:A):dataset[A] = a.asInstanceOf[dataset[A]]

  trait dataset[+A <: dataset[_]] {
    def isEmpty:Boolean
    def flatten[B<:dataset[_]](implicit ev: A<:< dataset[B]):dataset[B] = if (isEmpty) throw this.asInstanceOf[DatasetError[A]].value else this.asInstanceOf[dataset[B]]
    val context: contexttype
    def withContext(ctx: contexttype): dataset[A]
    val id: idtype
  }

  trait axiom[ A <: axiom[A]] extends dataset[A] {
    override final val id = buildId[this.type]
    override val context: contexttype = Map()
    override final def isEmpty = false
    override def withContext(ctx: contexttype): dataset[A] = null
    val get:A = this.asInstanceOf[A]
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


  case class DatasetError[+A<:dataset[_]](value:Error,par_id:idtype) extends dataset[A] {
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

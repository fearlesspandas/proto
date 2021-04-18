package Typical.core;

import Typical.core.Id._

import scala.reflect.runtime.universe._
import grammar._
package object typeable {
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
  private[core] def build[A:TypeTag]:A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    //val test = c1.info.resultType.paramLists
    val c2 = cm.reflectConstructor(c1)
    c2.apply().asInstanceOf[A]
  }
  private[core] def buildInferred[A](implicit create: (TypeTag[A]) => A,a:TypeTag[A]):A = {
    create(a)
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
  type contexttype = Map[idtype,dataset[_]]
  type idtype = Any


  trait produces[+T]{
    val value:T// = null.asInstanceOf[T]
  }

  trait InitialType[-T<:dataset[_]]{
    val terminalValue:dataset[_>:T<:dataset[_]] = null
  }

  trait dataset[+A <: dataset[_]]{
    val context:contexttype
    def withContext(ctx:contexttype):dataset[A]
    def id:idtype
  }

  trait axiom[A <: axiom[A,T],T] extends dataset[A] with produces[T]{
    override final val id = this.getClass.getTypeName
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[A] = null
    def withValue(newVal:T):A
  }

  trait modelBase[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output]  {self =>
    def iterate(src:dataset[dependencies]):Option[output]
    //override final val id = this.toString
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[output] = null
  }

    trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends modelBase[dependencies,output]{
      override final val id = this.toString.filterNot(c => c == ')' || c == '(')
    }
  //
      trait Mmodel[dep<:dataset[_],A<:dataset[_],T] extends modelBase[dep,A] with produces[T]{
        val f:dataset[dep] => (T,A)
      }
    class MmodelInstance[dep<:dataset[_],T,A<:axiom[A,T]](val f:dataset[dep] => (T,A),val value:T)(implicit tagA:TypeTag[A]) extends Mmodel[dep,A,T] {

      override def id: idtype = build[A].id

      override def iterate(src: dataset[dep]): Option[A] = {
        val next = f(src)
        Some(next._2.withValue(next._1))
      }
    }
//      def gen[T,dep,dat<:dataset[_]](func:dataset[dep] => (T,dat)):model[dep,dat] with produces[T]= {
//          new Mmodel[dep,dat,T] {
//            override final val id = build[dat].id.toString
//            override val f: dataset[dep] => (T, dat) = func
//            override val value: T = null.asInstanceOf[T]
//
//            override def iterate(src: dataset[dep]): Option[dat] = (func(src)._2)
//          }
//      }

  trait directive[dependencies <: dataset[_], +self <:directive[_,self]] extends model[dependencies,self] with produces[dataset[dependencies]]{

  }
  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = data[A](ctx)
    override val id: idtype = null.asInstanceOf[idtype]
    def dataset = this.asInstanceOf[dataset[A]]
  }

}
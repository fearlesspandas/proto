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
  def buildId[A:TypeTag]:idtype = typeTag[A].tpe.typeSymbol.toString()
 
  // can build an instance that from type info alone that does not take arguments
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

  def validateResult[A<:dataset[_]](src:dataset[_])(dat:Option[A])(implicit taga:TypeTag[A]):Option[A] = dat match {
    case Some(res) => Some(res)
    case _ =>
      throw new Error(s"No value for ${buildId[A]} found")

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

  trait failsWith[+T]{
    val err:T
  }
  trait Id[+A<:dataset[_]]{
    val dat:A
  }
  trait InitialType[-T<:dataset[_]]{
    val terminalValue:dataset[_>:T<:dataset[_]] = null
  }

  trait dataset[+A <: dataset[_]]{
    val context:contexttype
    //val errorMap:Map[idtype,Error]
    val IdRelations:Map[idtype,idtype]
    def withContext(ctx:contexttype):dataset[A]
    //def withError()
    val id:idtype
  }

  trait axiom[T,A <: axiom[T,A]] extends dataset[A] with produces[T]{
    override final val id = buildId[this.type]
    override val context: contexttype = Map()
    //override val errorMap: Map[idtype, Error] = Map()
    override val IdRelations: Map[idtype, idtype] = Map(id -> id)
    override def withContext(ctx: contexttype): dataset[A] = null
    def withValue(newVal:T):A
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output]  {self =>
    def iterate(src:dataset[dependencies]):Option[output]
    override final val id = buildId[this.type]
    //override val errorMap: Map[idtype, Error] = Map()
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[output] = null
  }

  trait FinalModel[-dependencies <: dataset[_], +output <: dataset[_]] extends model[dependencies,output]{
    override val IdRelations: Map[idtype, idtype] = Map(id -> id)
  }

  trait Mmodel[dep<:dataset[_],A<:dataset[_],T] extends model[dep,A] with produces[T]{
    val f:dataset[dep] => (T,A)
  }


  trait modelUnion[dependencies <: dataset[_], +self <:modelUnion[_,self]] extends FinalModel[dependencies,self] with produces[dataset[dependencies]]{
    def next(src:dataset[dependencies]):Option[dataset[dependencies]]
    def apply(value:dataset[dependencies]):self
    final override def iterate(src: dataset[dependencies]): Option[self] = for{
      d <- next(src)
    } yield apply(d)
  }
  case class data[A<:dataset[_]](override val context:contexttype) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = {
     // val currErrors = this.errorMap
      new data[A](ctx)
    }
    override val id: idtype = null.asInstanceOf[idtype]
    override val IdRelations: Map[idtype, idtype] = context.values.foldLeft(Map[idtype,idtype]())((a,c) => a ++ c.IdRelations)
    def dataset = this.asInstanceOf[dataset[A]]

    //override val errorMap: Map[idtype, Error] = context.values.map{case d:failsWith[_] => d.id -> d.err; case d => d.id -> new Error(s"Failed to locate ${d.id}")}.toMap
  }

}
package Typical.core;


import scala.reflect.runtime.universe.{TypeTag,typeTag,runtimeMirror,typeOf,termNames}
import grammar._
package object dataset {
  type contexttype = Map[idtype, dataset[_]]

  type idtype = Any


  def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

  def m[T: TypeTag, S: TypeTag](x: T, y: S): Boolean = {
    val leftTag = typeTag[T]
    val rightTag = typeTag[S]
    leftTag.tpe <:< rightTag.tpe
  }

  def buildId[A: TypeTag]: idtype = typeTag[A].tpe.typeSymbol.toString()

  def buildIdLor[A: TypeTag](rel:Map[idtype,idtype], next:Option[idtype] = None,curr:Option[idtype] = None): idtype = next match {
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
  def build[A: TypeTag]: A = {
    val m = runtimeMirror(getClass.getClassLoader)
    val classThing = typeOf[A].typeSymbol.asClass
    val cm = m.reflectClass(classThing)
    val c1 = typeOf[A].decl(termNames.CONSTRUCTOR).asMethod
    val c2 = cm.reflectConstructor(c1)
    c2.apply().asInstanceOf[A]
  }
  def idLess(a:idtype,b:idtype):Boolean = {
    val baseA = removeSpecialChars(a.toString.split("with")).toSet
    val baseB = removeSpecialChars(b.toString.split("with")).toSet
    //val diff = baseB.diff(baseA)
    baseA.subsetOf(baseB)
  }
  def idcomb(a:idtype,b:idtype):idtype = a.toString + b.toString
  def removeSpecialChars(st:Seq[String]):Seq[String] = st.map(
    _
      .replaceAll(".package","")
      .replaceAll("<","")
      .replaceAll(">","")
      .replaceAll(" ","")
  )
  def buildInferred[A](implicit create: (TypeTag[A]) => A, a: TypeTag[A]): A = {
    create(a)
  }



  abstract class  Id[+A <: dataset[_]](implicit tag:TypeTag[A]) {
    val baseId = buildId[A]
  }

  trait InitialType[-T <: dataset[_]] {
    val terminalValue: dataset[_ >: T <: dataset[_]] = null
  }
  def apply[A<:dataset[_]](a:A):dataset[A] = a.asInstanceOf[dataset[A]]


  trait produces[+T]{
   val value:T
  }

  trait dataset[+A <: dataset[_]] {
    val context: contexttype
    val relations:Map[idtype,idtype]
    def isEmpty:Boolean
    def fold[B<:dataset[_]](ifEmpty: DatasetError[A] => dataset[B])(f: dataset[A] => dataset[B]): dataset[B] = if (isEmpty) ifEmpty(this.asInstanceOf[DatasetError[A]]) else f(this)
    def withContext(ctx: contexttype): dataset[A]
    def withRelations(rel:Map[idtype,idtype]):dataset[A]
  }

  trait axiom[ A <: axiom[A]] extends dataset[A] {
    override val context: contexttype = Map()
    override final def isEmpty = false
    override def withContext(ctx: contexttype): dataset[A] = null
    override val relations = Map()
    override def withRelations(rel:Map[idtype,idtype]):dataset[A] = DatasetError[A](new Error("No withContext method available"))
  }

  trait model[-dependencies <: dataset[_], +output <: dataset[_]] extends dataset[output] with Function[dataset[dependencies],dataset[output]] {
    self =>
    override final def isEmpty: Boolean = false
    override val context: contexttype = Map()
    override def withContext(ctx: contexttype): dataset[output] = DatasetError[output](new Error("No withContext method available"))
    override val relations = Map()
    override def withRelations(rel:Map[idtype,idtype]):dataset[output] = null
  }

  trait index[ self <: dataset[_]] extends model[self,self]{
    def apply():dataset[self]
    override def apply(src:dataset[self]):dataset[self] = apply()
  }

//  abstract class implicitModel[-dep<:dataset[_],+out<:dataset[_]](implicit tagdep:TypeTag[dep],tagout:TypeTag[out]) extends model[dep,out]{
//    val id_l = buildId[dep]
//    val id_r = buildId[out]
//    val combid = idcomb(id_l,id_r)
//  }


  case class DatasetError[+A<:dataset[_]](value:Error*) extends dataset[A] {
    override val context:contexttype = Map()
    override val relations: Map[idtype, idtype] = Map()
    override final def isEmpty: Boolean = true
    override def withContext(ctx: contexttype): dataset[A ] = new DatasetError[A](this.value:_*){
      override val context = ctx
    }
    def append(newvalue:Error *):DatasetError[A] = new DatasetError[A](this.value ++ newvalue :_*  )
    override def withRelations(rel: Map[idtype, idtype]): dataset[A] = new  DatasetError[A](this.value:_*){
      override val relations = rel
    }
  }

  case class data[A <: dataset[_]](override val context: contexttype = Map(),relations:Map[idtype,idtype] = Map()) extends dataset[A] {
    override def withContext(ctx: contexttype): dataset[A] = {
      val relationsHasErr = ctx.values.exists(_.isInstanceOf[DatasetError[_]])
      if(relationsHasErr) {
        DatasetError[A]()
      }
      else
        new data[A](ctx,this.relations)
    }
    override final def isEmpty: Boolean = false
    override def withRelations(rel: Map[idtype, idtype]): dataset[A] = data(this.context,rel)
  }

}

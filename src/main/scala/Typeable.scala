package Typical.core;

import scala.reflect.{ClassTag, classTag}

import scala.collection.immutable.HashMap
import scala.reflect.runtime.{universe => ru}
import ru._
object Typeable {
  type modelType[init,self<:model[_,self] with reset[init,self] with InitialType[init,self]] = model[_,self] with reset[init,self] with InitialType[init,self]
  type queryset[init] = dataset[_] with InitialType[init,_]
  trait wrappedArgs[A]
  val m = ru.runtimeMirror(getClass.getClassLoader)
  def build[A](implicit tagA:ClassTag[A],ttaga:ru.TypeTag[A]):A = {
    //classOf[A].newInstance().asInstanceOf[A]
    val classPerson = ru.typeOf[A].typeSymbol.asClass
    tagA.runtimeClass.newInstance().asInstanceOf[A]

//    val classType = typeOf[A].typeSymbol.asClass
//    val baseClassType = typeOf[wrappedArgs[_]].typeSymbol.asClass
//    val baseType = internal.thisType(classType).baseType(baseClassType)
//    baseType.typeArgs.head

    //    //classPerson.asMethod
//    val cm = m.reflectClass(classPerson)
//    ru.RuntimeClassTag[A]
//    val ctor = ru.typeOf[A].decl(ru.termNames.CONSTRUCTOR).asMethod
//    val ctorm = cm.reflectConstructor(ctor)
//    ctorm.apply().asInstanceOf[A]
//    val thing = classTag[A]
//      .runtimeClass
//      val res = thing.newInstance()
//        val res2 = res
//      .asInstanceOf[A]
//    res2
  }
//  def buildRec[A[_]]:A[_] = {
//
//  }
  def buildName[A](implicit tagA:ClassTag[A]):String = classTag[A]
    .runtimeClass
    .getSimpleName()

  //Data Accessor/consistency maintainer
  trait provider[-U] {
    val statefulmap:Map[String,Any]
    val statestore:Map[String,Seq[Any]]
    def put(s: String, a: Any): provider[U] = {
      class temp(override val statefulmap:Map[String,Any],override val statestore: Map[String, Seq[Any]]) extends provider[U]
      val oldmap = this.statestore.getOrElse(s,Seq())
      val newmap = this.statefulmap.updated(s,a)
      new temp(newmap,this.statestore.updated(s,a +: oldmap))
    }
    def get[A](implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = this.statefulmap.get(name)
      ret
    }
    def getAs[U<:dataset[_],as](implicit tag:ClassTag[U],ttag:ru.TypeTag[U]):as = {
      this.statefulmap.get(build[U].name).collect({case a:as => a}).getOrElse(null).asInstanceOf[as]
    }
    def getStateAs[U<:dataset[_],as](n:Int)(implicit tag:ClassTag[U],ttag:ru.TypeTag[U]):as = {
      val key = build[U].name
      if (n < this.statestore(key).size) this.statestore(key)(n).asInstanceOf[as] else null.asInstanceOf[as]
    }
  }

  trait InitialType[A,+B<:dataset[_]]{
    type tpe = A
    val value:A
    def apply[U<:dataset[_] with InitialType[tpe,_]](initval: dataset[U] with InitialType[tpe,_],prov:provider[_]): dataset[B with U] with InitialType[tpe,_]
    def applyFromData[U<:dataset[_] with InitialType[A,_]](initial: A,prov:provider[_]): dataset[B with U] with InitialType[A,_]
    implicit val prov:provider[_]
  }
  trait dataset[+A <: dataset[_]]{
    val initialVal:Any
    val name:String

    def dataprovider():provider[_]
    def clone(p:provider[_] = this.dataprovider()):dataset[A]
  }

  trait ax[A <: ax[A]] extends dataset[A]{
    implicit val tag:ClassTag[A]
    val name:String = classTag[A].runtimeClass.getSimpleName
  }

  trait reset[initType,A<:model[_,A] with reset[initType,A] with InitialType[initType,_]] {
    def reset(initial: dataset[_] with InitialType[initType,_]): dataset[A] with reset[initType,A]
    def reset2(initial: initType): dataset[A] with reset[initType,A]
  }
  trait model[-dependencies <: dataset[_], output <: model[_,output]] extends dataset[output] {
    implicit val tag:ClassTag[output]
    val name:String = classTag[output].runtimeClass.getSimpleName
    implicit val iterateFrom:dataset[dependencies] => dataset[output]
  }

  trait number extends provider[number] {
    override lazy val statestore: Map[String, Seq[Any]] = HashMap()
    override lazy val statefulmap = HashMap[String,Any]()
  }

}

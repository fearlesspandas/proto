package Typical.core;

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._

import scala.collection.mutable
import scala.collection.immutable.HashMap
import Typical.implicits.implicits._

object Typeable {

  def build[A](implicit tagA:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  def buildName[A](implicit tagA:ClassTag[A]):String = classTag[A].runtimeClass.getSimpleName()

  //Data Accessor/consistency maintainer
  trait provider[-U] {
    //val mymap:Map[String,Any]
    val statefulmap:Map[String,Any]
    def put(s: String, a: Any): provider[U] ={
      class temp(override val statefulmap:Map[String,Any]) extends provider[U]
      new temp(this.statefulmap.updated(s,a))
    }
    def get[A](implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = this.statefulmap.get(name)
      ret
    }
     def getOther[U<:dataset[_],as](implicit tag:ClassTag[U]):as = {
      val res = this.statefulmap.get(build[U].name).collect({case a:as => a}).getOrElse(null).asInstanceOf[as]
      res
    }
  }

  trait InitialType[A,+B<:dataset[_]]{
    type tpe = A
    val typedInitVal:A
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
    val refmap = HashMap[String,Any](("balance",1000),("baserate",1),("TaxBurden",0))
    override val statefulmap = HashMap[String,Any]()
    def getfrommap[A](m:Map[String,Any])(implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = m.get(name)
      ret
    }
    def getStatefulfrommap(s:String,m:Map[String,Any]):Option[Any] = m.get(s).collect({case i:Double => i; case s:Seq[_] => Some(s); case _ => None})
    def getOtherfrommap[U<:dataset[_],as](m:Map[String,Any])(implicit tag:ClassTag[U]):as = {
      val res = m.get(build[U].name).collect({case a:as => a}).getOrElse(null).asInstanceOf[as]
      res
    }
    def putInMap(s: String, a: Any,m:Map[String,Any]): Map[String,Any] =  {
      m.updated(s,a)
    }
  }

}

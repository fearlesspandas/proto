package Typical.implicits

import Typical.core.Typeable.{InitialType, _}

import scala.reflect.ClassTag

object implicits {
  trait Calcing[B,A <: dataset[_]]{
    //def calc[U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U]
  }
  class CalcGeneric[A <: dataset[_] with InitialType[_,_]](a: dataset[A])(implicit tagA:ClassTag[A]){ // f is not being resolved to anything but the identity function
    val instanceA = build[A]
    val prov = a.dataprovider()
    def calc[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U] = {
      val instance2 = build[U]
      val res = instance2.iterateFrom(a).asInstanceOf[U]
      instance2.applyFromData(res.typedInitVal,a.dataprovider().put(res.name,res.initialVal)).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]
      //instance2.apply(res).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]
    }
    def calcDouble[U >: A <: model[A, U] with dataset[_] with InitialType[Double, U]](implicit tagu: ClassTag[U]) =  this.calc[Double,U]
    def calcSeq[U >: A <: model[A, U] with dataset[_] with InitialType[Seq[_], U]](implicit tagu: ClassTag[U]) =  this.calc[Seq[_],U]
  }

  implicit class calcable[A<:dataset[_] with InitialType[Double,_]](a:dataset[A])(implicit tagA:ClassTag[A]) extends CalcGeneric[A](a) //with CalcGeneric[Seq[_],A](a)


  implicit class Fetcher[A <: dataset[_] with InitialType[_,_]](a: dataset[A])(implicit tagA:ClassTag[A]){
    val prov = a.dataprovider()
    def fetch[B,U >: A <: dataset[U] with InitialType[B,U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[B,U] = {
      val instanceu = build[U]
      instanceu.applyFromData(prov.getOther[U,B],prov).asInstanceOf[U]
    }
    def fetchDouble[U >: A <: dataset[U] with InitialType[Double,U]](implicit tagu: ClassTag[U]) = this.fetch[Double,U]
    def fetchSeq[U >: A <: dataset[U] with InitialType[Seq[_],U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[Seq[_],U] = this.fetch[Seq[_],U]
  }


  implicit class AlgebraProvider[A<:dataset[_] with InitialType[Double,_]](a:dataset[A] with InitialType[Double,_])(implicit prov:provider[_],classTag: ClassTag[A]) {
    val instance = build[A]
    type tp = instance.tpe
    def +[U<:dataset[U] with InitialType[Double,U]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal + a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def -[U<:dataset[U] with InitialType[Double,U]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal - a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def *[U<:dataset[U] with InitialType[Double,U]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal * a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def /[U<:dataset[U] with InitialType[Double,U]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal / a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]

    //add any custom operations to dataset here
    //Could be algebraic operations outside of those defined for Double
    //or any custom operations
  }

  implicit class SequenceOps[A<:dataset[_] with InitialType[Seq[Double],A]](b:dataset[A] with InitialType[Seq[Double],A])(implicit prov:provider[A],classTag: ClassTag[A]) {
    def append[U <: dataset[_] with InitialType[Double, U]](u: U): dataset[A] = b.applyFromData(b.typedInitVal :+ u.typedInitVal,prov)
  }
  implicit class ToResetter[initType,A<:dataset[_]](f:dataset[A] => dataset[_] with InitialType[initType,_]){
    def set[B <:model[_,B] with reset[initType,B] with InitialType[initType,B]](implicit tag:ClassTag[B]):dataset[A] => dataset[B] with reset[initType,B] = (d:dataset[A]) => build[B].reset(f(d))
  }
//  implicit class ToResetter2[initType,A<:dataset[_]](f:dataset[A] => initType){
//    def set[B <:model[_,B] with reset[initType,B] with InitialType[initType,B]](implicit tag:ClassTag[B]):dataset[A] => dataset[B] with reset[initType,B] = (d:dataset[A]) => build[B].reset2(f(d))
//  }
  implicit class MapIterator(p:provider[_]){
    def register[A<:dataset[_] with InitialType[_,_]](implicit tag:ClassTag[A]):provider[_] = {
      val instA = build[A]
      p.put(instA.name,instA.initialVal)
    }
  }
}

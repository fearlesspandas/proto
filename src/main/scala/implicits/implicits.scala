package Typical.implicits

import Typical.core.Typeable._

import scala.reflect.ClassTag

object implicits {
  implicit class Calc[A <: dataset[_] with InitialType[_,_]](a: dataset[A])(implicit tagA:ClassTag[A],prov:provider[A]){ // f is not being resolved to anything but the identity function
    val instance = build[A]
    type tp = instance.tpe
    def calc[U>:A<:model[A,U] with dataset[_] with InitialType[instance.tpe,U]](implicit tagu:ClassTag[U]): dataset[A with U] = {
      val instance2 = build[U]
      val res = instance2.iterateFrom(a).asInstanceOf[U]
      instance2.apply(res).asInstanceOf[dataset[A with U]]
    }
    def fetchDouble[U >: A <: dataset[U] with InitialType[Double,U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[Double,U] = {
      val instanceu = build[U]
      instanceu.applyFromData(prov.getOther[U,instanceu.tpe]).asInstanceOf[U]
    }
    def fetchSeq[U >: A <: dataset[U] with InitialType[Seq[Double],U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[Seq[Double],U] = {
      val instanceu = build[U]
      instanceu.applyFromData(prov.getOther[U,instanceu.tpe]).asInstanceOf[U]
    }
  }
  implicit class DataProvider[A<:dataset[_] with InitialType[Double,A]](a:dataset[A] with InitialType[Double,A])(implicit prov:provider[A],classTag: ClassTag[A]) {
    val instance = build[A]
    type tp = instance.tpe
    def +[U<:dataset[U] with InitialType[Double,U]](u:U):dataset[A] with InitialType[Double,A] = a.applyFromData(u.typedInitVal + a.initialVal.asInstanceOf[Double])
//      u.typedInitVal match {
//        case d:Double => a.apply(a.initialVal.asInstanceOf[Double] + u.initialVal.asInstanceOf[Double])
//        case _ => a
//      }

    //def -[U<:dataset[_] with InitialType[tp]](u:U):dataset[A] = a.apply(a.initialVal.asInstanceOf[Double] - u.initialVal.asInstanceOf[Double])
    def *[U<:dataset[_] with InitialType[Double,U]](u:U):dataset[A] with InitialType[Double,A] = a.applyFromData(a.initialVal.asInstanceOf[Double] * u.initialVal.asInstanceOf[Double])
    def /[U<:dataset[_] with InitialType[Double,U]](u:U):dataset[A] with InitialType[Double,A] = a.applyFromData(a.initialVal.asInstanceOf[Double] / u.initialVal.asInstanceOf[Double])

    //add any custom operations to dataset here
    //Could be algebraic operations outside of those defined for Double
    //or any custom operations
  }

  implicit class SequenceOps[A<:dataset[_] with InitialType[Seq[Double],A]](b:dataset[A] with InitialType[Seq[Double],A])(implicit prov:provider[A],classTag: ClassTag[A]) {
    def append[U <: dataset[_] with InitialType[Double, U]](u: U): dataset[A] = b.applyFromData(b.typedInitVal :+ u.typedInitVal)
  }
}

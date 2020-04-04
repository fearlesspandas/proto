package Typical.implicits

import Typical.core.Typeable.{InitialType, _}
import Typical.impl.bind

import scala.reflect.ClassTag

object implicits {
  trait Calcing[B,A <: dataset[_]]{
    //def calc[U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U]
  }
  def calcGeneral[B,dep<:dataset[_],target >: dep <: model[dep, target] with dataset[_] with InitialType[B,target]](a:dataset[dep])(ctx:provider[_] = a.dataprovider())(implicit tagu: ClassTag[target]): dataset[dep with target] with InitialType[B, dep with target] = {
    //println("calling calc")
    val instance2 = build[target]
    val res = instance2.iterateFrom(a.clone(ctx)).asInstanceOf[target]
    instance2.applyFromData(res.typedInitVal, ctx.put(res.name, res.initialVal)).asInstanceOf[dataset[dep with target] with InitialType[B,dep with target]]

  }

  trait Calcer[
    A <: dataset[_] with InitialType[_,_],
    other<:dataset[_]
  ]{
    def calc[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](ctx:provider[_])(implicit tagu: ClassTag[U]): dataset[A with U with other] with InitialType[B, A with U with other]
    def calc[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[A with U with other] with InitialType[B, A with U with other]
  }
  implicit class CalcGeneric[A <: dataset[_] with InitialType[_,_]](a: dataset[A])(implicit tagA:ClassTag[A]) extends Calcer[A,A]{
    val prov = a.dataprovider()
    override def calc[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](ctx:provider[_] = this.prov)(implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U] = {
      calcGeneral[B,A,U](a)(ctx).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]

      //instance2.apply(res).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]
    }
    override def calc[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U ] = {
      this.calc[B,U](this.prov)
      //instance2.apply(res).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]
    }
    def calcWithBinder[B, U >: A <: model[A, U] with dataset[_] with InitialType[B, U],binder>:A<:bind[B,binder,_>:A,U]](ctx: provider[_])(implicit tagu: ClassTag[U],tagbinder:ClassTag[binder]): dataset[A with U with binder] with InitialType[B, A with U with binder] = {
      val x1 = calcGeneral[B,A with U with binder,binder](a.asInstanceOf[dataset[A with U with binder]])(ctx)
      calcGeneral[B,A,U](x1)(x1.prov)
    }
    def calcWithBindings[B, U >: A <: model[A, U] with dataset[_] with InitialType[B, U],binder>:A<:bind[B,binder,_>:A,U]]()(implicit tagu:ClassTag[U],tagbinder:ClassTag[binder]): dataset[A with U with binder] with InitialType[B, A with U with binder] = this.calcWithBinder[B,U,binder](this.prov)
    def calcIter[B,U >: A <: model[A, U] with dataset[_] with InitialType[B, U]](n:Int = 1)(implicit tagu: ClassTag[U]): dataset[A with U] with InitialType[B, A with U] = {
      (0 until n).foldLeft(this.a)( (acc,curr) => this.calc[B,U](acc.dataprovider().put(acc.name,acc.initialVal))).asInstanceOf[dataset[A with U] with InitialType[B, A with U]]
    }
    def calcDouble[U >: A <: model[A, U] with dataset[_] with InitialType[Double, U]](implicit tagu: ClassTag[U]) =  this.calc[Double,U]()
    def calcSeq[U >: A <: model[A, U] with dataset[_] with InitialType[Seq[_], U]](implicit tagu: ClassTag[U]) =  this.calc[Seq[_],U]()
  }

  implicit class CalcWithBind[
    initType,
    other<:dataset[_],
    dep<:dataset[_],
    M <:model[dep,M] with InitialType[_,M],
    T<:bind[initType,T,dep,M]
  ](a: dataset[bind[initType,T,dep,M] with M with dep with other])(implicit tagM:ClassTag[M],tagt:ClassTag[T],tagdep:ClassTag[dep]) {
    val isbinded = true
    val prov = a.dataprovider()
    def calcWithBinding[B, U >: M <: model[T with M with dep, U] with dataset[_] with InitialType[B, U]](ctx: provider[_])(implicit tagu: ClassTag[U]): dataset[T with M with dep with other with U] with InitialType[B, T with M with dep with other with U] = {
      val x1 = calcGeneral[B,dep with other with M with T,U](a.asInstanceOf[dataset[M with dep with other with T]])(ctx)
      calcGeneral[initType,dep with M with T with U, T](x1)(x1.prov)
        .asInstanceOf[dataset[T with M with dep with other with U] with InitialType[B, T with M with dep with other with U]]
    }
    def calcWithBinding[B, U >: M <: model[T with M with dep, U] with dataset[_] with InitialType[B, U]](implicit tagu: ClassTag[U]): dataset[T with M with dep with other with U] with InitialType[B, T with M with dep with other with U] = this.calcWithBinding[B,U](this.prov)
  }

  implicit class Fetcher[A <: dataset[_] with InitialType[_,_]](a: dataset[A])(implicit tagA:ClassTag[A]){
    val prov = a.dataprovider()
    def fetch[B,U >: A <: dataset[U] with InitialType[B,U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[B,U] = {
      val instanceu = build[U]
      instanceu.applyFromData(prov.getAs[U,B],prov).asInstanceOf[U]
    }
    def fetchFromState[B,U >: A <: dataset[U] with InitialType[B,U]](n:Int)(implicit tagu: ClassTag[U]): dataset[U] with InitialType[B,U] = {
      val instanceu = build[U]
      instanceu.applyFromData(prov.getStateAs[U,B](n),prov).asInstanceOf[U]
    }
    def fetchDouble[U >: A <: dataset[U] with InitialType[Double,U]](implicit tagu: ClassTag[U]) = this.fetch[Double,U]
    def fetchSeq[U >: A <: dataset[U] with InitialType[Seq[_],U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[Seq[_],U] = this.fetch[Seq[_],U]
    def fetchBool[U >: A <: dataset[U] with InitialType[Boolean,U]](implicit tagu: ClassTag[U]): dataset[U] with InitialType[Boolean,U] = this.fetch[Boolean,U]
  }


  implicit class AlgebraProvider[A<:dataset[_] with InitialType[Double,_]](a:dataset[A] with InitialType[Double,_])(implicit prov:provider[_],classTag: ClassTag[A]) {
    def +[U<:dataset[_] with InitialType[Double,_]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal + a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def -[U<:dataset[_] with InitialType[Double,_]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](a.typedInitVal - u.typedInitVal ,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def *[U<:dataset[_] with InitialType[Double,_]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U](u.typedInitVal * a.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def /[U<:dataset[_] with InitialType[Double,_]](u:U):dataset[A with U] with InitialType[Double,_] = a.applyFromData[U]( a.typedInitVal / u.typedInitVal,prov).asInstanceOf[dataset[A with U] with InitialType[Double,_]]
    def +(u:Double):dataset[A] with InitialType[Double,_] = a.applyFromData(u + a.typedInitVal,prov).asInstanceOf[dataset[A] with InitialType[Double,_]]
    def -(u:Double):dataset[A] with InitialType[Double,_] = a.applyFromData(a.typedInitVal -u ,prov).asInstanceOf[dataset[A] with InitialType[Double,_]]
    def *(u:Double):dataset[A] with InitialType[Double,_] = a.applyFromData(u * a.typedInitVal,prov).asInstanceOf[dataset[A] with InitialType[Double,_]]
    def /(u:Double):dataset[A] with InitialType[Double,_] = a.applyFromData(a.typedInitVal/u ,prov).asInstanceOf[dataset[A] with InitialType[Double,_]]
    //add any custom operations to dataset here
    //Could be algebraic operations outside of those defined for Double
    //or any custom operations
  }

  implicit class SequenceOps[A<:dataset[_] with InitialType[Seq[Double],A]](b:dataset[A] with InitialType[Seq[Double],A])(implicit prov:provider[A],classTag: ClassTag[A]) {
    def append[U <: dataset[_] with InitialType[Double, U]](u: U): dataset[A] = b.applyFromData(b.typedInitVal :+ u.typedInitVal,prov)
  }
  implicit class ToResetter[initType,A<:dataset[_]](f:dataset[A] => dataset[_] with InitialType[initType,_]){
    //val instanceA = build[A]
    //val prov = instanceA.dataprovider()
    def set[B <:model[_,B] with reset[initType,B] with InitialType[initType,B]](implicit tag:ClassTag[B]):dataset[A] => dataset[B] with reset[initType,B] = (d:dataset[A]) => build[B].reset(f(d))
  }
//  implicit class ToResetter2[initType,A<:dataset[_],B <: dataset[_] with InitialType[initType,B]](f:dataset[A] => initType){
//    def set[B <:model[_,B] with reset[initType,B] with InitialType[initType,B]](implicit tag:ClassTag[B]):dataset[A] => dataset[B] with reset[initType,B] = (d:dataset[A]) => build[B].reset(f(d))
//  }
  implicit class ToResetter2[initType,A<:dataset[_]](f:dataset[A] => initType){
    def set[B <:model[_,B] with reset[initType,B] with InitialType[initType,B]](implicit tag:ClassTag[B]):dataset[A] => dataset[B] with reset[initType,B] = (d:dataset[A]) => build[B].reset2(f(d))
  }
  implicit class ContextBuilder(p:provider[_]){
    def register[A<:dataset[_] with InitialType[_,_]](implicit tag:ClassTag[A]):provider[_] = {
      val instA = build[A]
      p.put(instA.name,instA.initialVal)
    }
  }
}

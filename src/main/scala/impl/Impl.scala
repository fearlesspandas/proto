//package Typical.core
//
//import Typical.core.Typeable._
//
//import scala.reflect.ClassTag
//
//import Typical.core.implicits._
//
//package object impl {
//
//  implicit object baseprovider extends number
//
//
//  case class axiom[B, A <: ax[A] with InitialType[B, A]](override val value: B)(override implicit val tag: ClassTag[A], provi: provider[_]) extends ax[A] with InitialType[B, A] {
//    type asAx = axiom[B,A]
//    override lazy val initialVal: Any = this.value
//    override lazy val prov: provider[_] = provi.put(this.name, this.initialVal)
//
//    def applyFromData[U <: dataset[_] with InitialType[B, _]](initial: B, p: provider[_]): dataset[A with U] with InitialType[B, _] = {
//      new axiom[B, A](initial)(tag, p).asInstanceOf[dataset[A with U] with InitialType[B, _]]
//      //(new temp).asInstanceOf[A]
//    }
//
//    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], prov: provider[_]): dataset[A with U] with InitialType[tpe, _] = this.applyFromData(initval.value, prov)
//
//    override def dataprovider(): provider[_] = this.prov
//
//    override def clone(p: provider[_]): dataset[A] = this.applyFromData(this.value, p)
//  }
//
//
//  case class data[+A <: dataset[_]](dprov: provider[_], override val value: Any = null)(implicit tag: ClassTag[A]) extends dataset[A] with InitialType[Any, A] {
//    override val name = ""
//    //val instance = build[A]
//    override val prov: provider[_] = dprov
//
//    override def dataprovider(): provider[_] = this.prov
//
//    override val initialVal: Any = value
//
//    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
//    override def applyFromData[U <: dataset[_] with InitialType[tpe, _]](initial: tpe, provi: provider[_]): dataset[A with U] with InitialType[tpe, _] = data[A with U](provi, initial)
//
//    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], provi: provider[_]): dataset[A with U] with InitialType[tpe, _] = this.applyFromData(initval.value, provi)
//
//    override def clone(p: provider[_]): dataset[A] = this.applyFromData(this.value, p)
//    def dataset:dataset[A] = this.asInstanceOf[dataset[A]]
//  }
//
//  class sim[
//    initType,
//    -dependencies <: dataset[_],
//    self <: model[_, self] with InitialType[initType, self]
//  ](
//     override val iterateFrom: dataset[dependencies] => dataset[self]
//   )
//   (
//     override val value: initType
//   )(
//     implicit override val tag: ClassTag[self], dprov: provider[_]
//   ) extends model[dependencies, self] with InitialType[initType, self]{
//    override lazy val initialVal: Any = this.value
//    override lazy val prov: provider[_] = dprov.put(this.name, this.initialVal)
//    def applyFromData[U <: dataset[_] with InitialType[initType, _]](initial: initType, p: provider[_]): dataset[self with U] with InitialType[initType, _] = {
//      new sim[initType, dependencies, self](iterateFrom)(initial)(tag, p).asInstanceOf[dataset[self with U] with InitialType[initType, _]]
//    }
//    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], p: provider[_]): dataset[self with U] with InitialType[initType, _] = this.applyFromData(initval.value, p)
//    override def reset2(initial: Any): dataset[self] = {
//      implicit val newprov = this.prov.put(this.name, initial)
//      new sim[initType, dependencies, self](iterateFrom)(initial.asInstanceOf[initType])( tag, this.prov.put(this.name, initial)).asInstanceOf[self]
//    }
//    //override def reset(initial: dataset[_] with InitialType[initType, _]): dataset[self] = this.reset2(initial.value)
//    override def dataprovider(): provider[_] = this.prov
//    override def clone(p: provider[_]): dataset[self] = this.applyFromData(this.value, p)
//  }
//
//  class rsim[
//    initType,
//    -dependencies <: dataset[self],
//    self <: model[_, self] with InitialType[initType, self]
//  ](
//     override val iterateFrom: dataset[dependencies] => dataset[self]
//   )(
//     override val value: initType
//   )(
//     implicit override val tag: ClassTag[self], prov: provider[_]
//   ) extends sim[initType, dependencies, self](iterateFrom)(value)(tag, prov) with InitialType[initType, self]
//
//
////  class SeqLooksConvergent[self <: SeqLooksConvergent[self, _, _, _] with model[_, self] with InitialType[Boolean, self],
////    dep <: dataset[_],
////    target <: model[dep, target] with InitialType[Double, target],
////    targetsum <: sum[targetsum, dep, target]
////  ](
////     eps: Double, N: Int
////   )(
////     implicit override val tag: ClassTag[self], tagu: ClassTag[target], tagsum: ClassTag[targetsum] //,ctx:provider[_]
////   ) extends LooksConvergent[self, dep with targetsum with target, targetsum](eps, N)
//
//  class LooksConvergent[
//    self <: LooksConvergent[self, _, _] with model[_, self] with InitialType[Boolean, self],
//    dep <: dataset[_],
//    target <: model[dep, target] with InitialType[Double, target]
//  ](
//     eps: Double, N: Int
//   )(
//     implicit override val tag: ClassTag[self], tagu: ClassTag[target] //,ctx:provider[_]
//   ) extends rsim[Boolean, dep with self with target, self](
//    (
//      (src: dataset[dep with self with target]) => {
//        val wasConvergent = src.fetchBool[self]
//        val lastval = src.fetchDouble[target]
//        val nextval = src.calcIter[Double, target](N).fetchDouble[target]
//        wasConvergent.value || scala.math.abs((lastval - nextval).value) < eps
//      }
//      ).set[self]
//  )(false)
//
////
////  class sum[
////    self <: sum[self, _, _] with model[_, self] with InitialType[Double, self],
////    dep <: dataset[_], target <: model[dep, target] with InitialType[Double, target]
////  ](implicit override  val tagtarget: ClassTag[target], tagself: ClassTag[self], dprov: provider[_]
////   ) extends bind[Double,self,dep,target](
////    (
////      (src: dataset[dep with self with target]) => {
////        val currsum = src.fetchDouble[self]
////        val nextval = src.fetchDouble[target]
////        currsum + nextval
////      }
////      ).set[self]
////  )(build[target].value)(tagtarget, tagself, dprov)
//
//
//  class bind[
//    initType,self <: bind[initType,self, _, _] with model[_, self] with InitialType[initType, self],
//    dep <: dataset[_],
//    target <: model[dep, target] with InitialType[_, target]
//  ](
//     override val iterateFrom: dataset[dep with self with target] => dataset[self])(override val value: initType)
//   (
//     implicit val tagtarget: ClassTag[target], tagself: ClassTag[self], dprov: provider[_]
//   ) extends rsim[initType,  dep with self with target,self](iterateFrom)(value)(tagself, dprov)
//
//
//  class flatten[
//    flatout,
//    dep<:dataset[_],
//    inputType,
//    A <: model[dep,A] with InitialType[inputType => flatout, A],
//    underlying<:dataset[_] with InitialType[_,_]
//  ](implicit tagA:ClassTag[A] )extends axiom[
//    (dataset[underlying with A with dep],inputType) => dataset[underlying with A with dep] with InitialType[inputType => flatout,underlying],
//    flatten[flatout,dep,inputType,A,underlying]
//  ](
//    (src:dataset[underlying with A with dep],o:inputType) => {
//      val res = src.calc[inputType => flatout,A].value(o)
//      src.include[inputType => flatout,A](_ => res)
//    }
//  )
//
//  implicit class flatten2[
//    underlyingdata<:dataset[_] with InitialType[_,_]
//  ](a:dataset[underlyingdata])
//  {
//    def toFlat[U>:underlyingdata<:model[underlyingdata,U] with InitialType[inputType => flatout, U],inputType,flatout](implicit tagu:ClassTag[U]) = {
//      new flatten[
//        flatout,
//        underlyingdata,
//        inputType,
//        U,
//        underlyingdata
//      ]
//
//    }
//  }
//  class combine[
//    self<:combine[self,A,_,typeA,B,_,typeB],
//    A<:rsim[typeA,A with depA,A],
//    depA<:dataset[_],
//    typeA,
//    B<:rsim[typeB,B with depB,B],
//    depB<:dataset[_],
//    typeB
//  ](implicit taga:ClassTag[A],tagb:ClassTag[B],tagself:ClassTag[self]) extends rsim[dataset[A with B],self with depA with depB with A with B,self](
//    ((src:dataset[self with depA with depB with A with B]) => {
//      //(src.calc[typeA,A].typedInitVal,src.calc[typeB,B].typedInitVal)
//      src.calc[typeA,A].calc[typeB,B].asInstanceOf[dataset[A with B]]
//    }).set[self]
//  )(null.asInstanceOf[dataset[A with B]])
//
//}
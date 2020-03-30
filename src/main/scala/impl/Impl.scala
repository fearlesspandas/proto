import Typical.core.Typeable._

import scala.reflect.ClassTag

import Typical.implicits.implicits._

package object impl {

  implicit object myprovider extends number


  case class axiom[B, A <: ax[A] with InitialType[B, A]](override val typedInitVal: B)(override implicit val tag: ClassTag[A], provi: provider[_]) extends ax[A] with InitialType[B, A] {
    override val initialVal: Any = this.typedInitVal
    override val prov: provider[_] = provi.put(this.name, this.initialVal)

    def applyFromData[U <: dataset[_] with InitialType[B, _]](initial: B, p: provider[_]): dataset[A with U] with InitialType[B, _] = {
      new axiom[B, A](initial)(tag, p).asInstanceOf[dataset[A with U] with InitialType[B, _]]
      //(new temp).asInstanceOf[A]
    }

    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], prov: provider[_]): dataset[A with U] with InitialType[tpe, _] = this.applyFromData(initval.typedInitVal, prov)

    override def dataprovider(): provider[_] = this.prov

    override def clone(p: provider[_]): dataset[A] = this.applyFromData(this.typedInitVal, p)
  }


  case class data[+A <: dataset[_]](dprov: provider[_], override val typedInitVal: Any = null)(implicit tag: ClassTag[A]) extends dataset[A] with InitialType[Any, A] {
    override val name = ""
    //val instance = build[A]
    override val prov: provider[_] = dprov

    override def dataprovider(): provider[_] = this.prov

    override val initialVal: Any = typedInitVal

    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
    override def applyFromData[U <: dataset[_] with InitialType[tpe, _]](initial: tpe, provi: provider[_]): dataset[A with U] with InitialType[tpe, _] = data[A with U](provi, initial)

    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], provi: provider[_]): dataset[A with U] with InitialType[tpe, _] = this.applyFromData(initval.typedInitVal, provi)

    override def clone(p: provider[_]): dataset[A] = this.applyFromData(this.typedInitVal, p)
  }

  class sim[initType, -A <: dataset[_], B <: model[_, B] with InitialType[initType, B] with reset[initType, B]](override val typedInitVal: initType)(implicit override val iterateFrom: dataset[A] => dataset[B], override val tag: ClassTag[B], dprov: provider[_]) extends model[A, B] with InitialType[initType, B] with reset[initType, B] {
    override val initialVal: Any = this.typedInitVal
    override val prov: provider[_] = dprov.put(this.name, this.initialVal)

    def applyFromData[U <: dataset[_] with InitialType[tpe, _]](initial: tpe, p: provider[_]): dataset[B with U] with InitialType[tpe, _] = {
      new sim[initType, A, B](initial)(iterateFrom, tag, p).asInstanceOf[dataset[B with U] with InitialType[tpe, _]]
    }

    override def apply[U <: dataset[_] with InitialType[tpe, _]](initval: dataset[U] with InitialType[tpe, _], p: provider[_]): dataset[B with U] with InitialType[tpe, _] = this.applyFromData(initval.typedInitVal, p)

    override def reset2(initial: initType): dataset[B] with reset[initType, B] = {
      //class temp extends sim[A,B](initial)
      implicit val newprov = this.prov.put(this.name, initial)
      new sim[initType, A, B](initial)(iterateFrom, tag, this.prov.put(this.name, initial)).asInstanceOf[B with reset[initType, B]]
    }

    override def reset(initial: dataset[_] with InitialType[initType, _]): dataset[B] with reset[initType, B] = this.reset2(initial.typedInitVal)

    override def dataprovider(): provider[_] = this.prov

    override def clone(p: provider[_]): dataset[B] = this.applyFromData(this.typedInitVal, p)
  }

  class recSim[initType, B <: model[_, B] with InitialType[initType, B] with reset[initType, B], -A <: dataset[B]](override val iterateFrom: dataset[A] => dataset[B] with reset[initType, B])(override val typedInitVal: initType)(implicit override val tag: ClassTag[B], prov: provider[_]) extends sim[initType, A, B](typedInitVal)(iterateFrom, tag, prov) with InitialType[initType, B] with reset[initType, B]


  class SeqLooksConvergent[self <: SeqLooksConvergent[self, _, _, _] with model[_, self] with InitialType[Boolean, self] with reset[Boolean, self], dep <: dataset[_], target <: model[dep, target] with InitialType[Double, target], targetsum <: sum[targetsum, dep, target]](
                                                                                                                                                                                                                                                                                 eps: Double, N: Int
                                                                                                                                                                                                                                                                               )(
                                                                                                                                                                                                                                                                                 implicit override val tag: ClassTag[self], tagu: ClassTag[target], tagsum: ClassTag[targetsum] //,ctx:provider[_]
                                                                                                                                                                                                                                                                               ) extends LooksConvergent[self, dep with targetsum with target, targetsum](eps, N)

  class LooksConvergent[self <: LooksConvergent[self, _, _] with model[_, self] with InitialType[Boolean, self] with reset[Boolean, self], dep <: dataset[_], target <: model[dep, target] with InitialType[Double, target]](
                                                                                                                                                                                                                              eps: Double, N: Int
                                                                                                                                                                                                                            )(
                                                                                                                                                                                                                              implicit override val tag: ClassTag[self], tagu: ClassTag[target] //,ctx:provider[_]
                                                                                                                                                                                                                            ) extends recSim[Boolean, self, dep with self with target](
    (
      (src: dataset[dep with self with target]) => {
        val wasConvergent = src.fetchBool[self]
        val lastval = src.fetchDouble[target]
        val nextval = src.calcIter[Double, target](N).fetchDouble[target]
        wasConvergent.typedInitVal || scala.math.abs((lastval - nextval).typedInitVal) < eps
      }
      ).set[self]
  )(false)


  class sum[self <: sum[self, _, _] with model[_, self] with InitialType[Double, self] with reset[Double, self], dep <: dataset[_], target <: model[dep, target] with InitialType[Double, target]]
  (
   implicit override  val tagtarget: ClassTag[target], tagself: ClassTag[self], dprov: provider[_]
  ) extends bind[Double,self,dep,target](
    (
      (src: dataset[dep with self with target]) => {
        val currsum = src.fetchDouble[self]
        val nextval = src.fetchDouble[target]
        currsum + nextval
      }
      ).set[self]
  )(0d)(tagtarget, tagself, dprov)


  class bind[initType,self <: bind[initType,self, _, _] with model[_, self] with InitialType[initType, self] with reset[initType, self], dep <: dataset[_], target <: model[dep, target] with InitialType[_, target]]
  (override val iterateFrom: dataset[dep with self with target] => dataset[self] with reset[initType, self])(override val typedInitVal: initType)
  (
  implicit val tagtarget: ClassTag[target], tagself: ClassTag[self], dprov: provider[_]
  ) extends recSim[initType, self, dep with self with target](iterateFrom)(typedInitVal)(tagself, dprov)


}

import Typical.core.Typeable._

import scala.reflect.ClassTag

package object impl {

  implicit object myprovider extends number


  case class axiom[B,A<:ax[A] with InitialType[B,A]](override val typedInitVal: B,initializing:Boolean = true)(override implicit val tag:ClassTag[A],override implicit var prov:provider[Nothing] ) extends ax[A] with InitialType [B,A]{
    override val initialVal:Any = this.typedInitVal
    if (this.prov.getStateful(this.name).isEmpty) this.prov.put(this.name,this.initialVal)
     def applyFromData[U<:dataset[_] with InitialType[B,_]](initial: B): dataset[A with U] with InitialType[B,_] = {
      new axiom[B,A](initial,false).asInstanceOf[dataset[A with U] with InitialType[B,_]]
      //(new temp).asInstanceOf[A]
    }

   override def apply[U<:dataset[_] with InitialType[tpe,_]](initval: dataset[U] with InitialType[tpe,_]): dataset[A with U] with InitialType[tpe,_] = this.applyFromData(initval.typedInitVal)
  }


  case class data[+A<:dataset[_]]() extends dataset[A] with InitialType[Any,A]{
    override val typedInitVal = null
    override val name = ""
    override var prov:provider[Nothing] = myprovider
    override val initialVal: Any = typedInitVal
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
    override def applyFromData[U<:dataset[_] with InitialType[tpe,_]](initial: tpe): dataset[A with U] with InitialType[tpe,_] = null

    override def apply[U<:dataset[_] with InitialType[tpe,_]](initval: dataset[U] with InitialType[tpe,_]): dataset[A with U] with InitialType[tpe,_] = null
  }

  class sim[initType,A<:dataset[_],B<:model[_,B] with InitialType[initType,B] with reset[initType,B]](override val typedInitVal: initType)(implicit override val iterateFrom: dataset[A] => dataset[B],override val tag:ClassTag[B]) extends model[A,B] with InitialType[initType,B] with reset[initType,B]{
    override var prov:provider[Nothing] = myprovider
    override val initialVal:Any = this.typedInitVal
     if (this.prov.getStateful(this.name).isEmpty)  this.prov.put(this.name,this.initialVal)
     def applyFromData[U<:dataset[_] with InitialType[tpe,_]](initial: tpe): dataset[B with U] with InitialType[tpe,_] = {
      new sim[initType,A,B](initial).asInstanceOf[dataset[B with U] with InitialType[tpe,_]]
    }
    override def apply[U<:dataset[_] with InitialType[tpe,_]](initval: dataset[U] with InitialType[tpe,_]): dataset[B with U] with InitialType[tpe,_] = this.applyFromData(initval.typedInitVal)
    override def reset2(initial: initType): dataset[B] with reset[initType,B] = {
      //class temp extends sim[A,B](initial)
      this.prov.put(this.name,initial)
      new sim[initType,A,B](initial).asInstanceOf[B with reset[initType,B]]
    }
    override def reset(initial: dataset[_] with InitialType[initType,_]): dataset[B] with reset[initType,B] = this.reset2(initial.typedInitVal)
  }

  class recSim[initType,B<:model[_,B] with InitialType[initType,B] with reset[initType,B],A<:dataset[B]](override val iterateFrom: dataset[A] => dataset[B] with reset[initType,B])(override val typedInitVal:initType)(implicit override val tag:ClassTag[B]) extends sim[initType,A,B](typedInitVal)(iterateFrom,tag) with InitialType [initType,B] with reset[initType,B]{
    //this.prov.put(this.name,this.initialVal)
  }
}

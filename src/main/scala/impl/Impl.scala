import Typical.core.Typeable._

import scala.reflect.ClassTag

package object impl {

  implicit object myprovider extends number


  case class axiom[B,A<:ax[A] with InitialType[B,A]](override val typedInitVal: B,initializing:Boolean = true)(override implicit val tag:ClassTag[A],override implicit var prov:provider[Nothing] ) extends ax[A] with InitialType [B,A]{
    override val initialVal:Any = this.typedInitVal
    if (this.prov.getStateful(this.name).isEmpty) this.prov.put(this.name,this.initialVal)
     def applyFromData(initial: B): dataset[A] with InitialType[B,A] = {
      new axiom[B,A](initial,false).asInstanceOf[A]
      //(new temp).asInstanceOf[A]
    }

    override def apply(initval: dataset[_] with InitialType[tpe,_]): dataset[A] with InitialType[tpe,A] = this.applyFromData(initval.typedInitVal)
  }


  case class data[initType,+A<:dataset[_]](override val typedInitVal:initType) extends dataset[A] with InitialType[initType,A]{
    override val name = ""
    override var prov:provider[Nothing] = myprovider
    override val initialVal: Any = typedInitVal
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
    override def applyFromData(initVal: tpe): dataset[A] with InitialType[tpe, A] = null

    override def apply(initialVal: dataset[_] with InitialType[tpe, _]): dataset[A] with InitialType[tpe, A] = null
  }

  class sim[initType,A<:dataset[_],B<:model[_,B] with InitialType[initType,B]](override val typedInitVal: initType,initializing:Boolean = false)(implicit override val iterateFrom: dataset[A] => dataset[B],override val tag:ClassTag[B]) extends model[A,B] with InitialType[initType,B]{
    override var prov:provider[Nothing] = myprovider
    override val initialVal:Any = this.typedInitVal
     if (this.prov.getStateful(this.name).isEmpty || this.initializing)  this.prov.put(this.name,this.initialVal)
     def applyFromData(initial: initType): dataset[B] with InitialType[initType,B]= {
      new sim[initType,A,B](initial).asInstanceOf[B]
    }
    override def apply(initial: dataset[_] with InitialType[initType,_]): dataset[B] with InitialType[initType,B] = this.applyFromData(initial.typedInitVal)
    def reset2(initial: initType): dataset[B] = {
      //class temp extends sim[A,B](initial)
      this.prov.put(this.name,initial)
      new sim[initType,A,B](initial,true).asInstanceOf[B]
    }
    def reset(initial: dataset[_] with InitialType[initType,_]): dataset[B] = this.reset2(initial.typedInitVal)
  }

  class recSim[initType,B<:model[_,B] with InitialType[initType,B],A<:dataset[B]](override val iterateFrom: dataset[A] => dataset[B])(override val typedInitVal:initType)(implicit override val tag:ClassTag[B]) extends sim[initType,A,B](typedInitVal)(iterateFrom,tag) with InitialType [initType,B]{
    //this.prov.put(this.name,this.initialVal)
  }
}

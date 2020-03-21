import Typical.core.Typeable.{ax, dataset, model, number, provider}

import scala.reflect.ClassTag

package object impl{

  implicit object myprovider extends number


  case class axiom[A<:ax[A]](override val initialVal: Double)(override implicit val tag:ClassTag[A],override implicit var prov:provider[Nothing] ) extends ax[A]{
    //override var prov:provider[Nothing] = myprovider
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
    override def apply(initial: Double): dataset[A] = {
      val res = this.initialVal + initial
      class temp extends axiom[A](initial)
      (new temp).asInstanceOf[A]
    }
  }


  case class data[+A<:dataset[_]](override val initialVal: Double) extends dataset[A]{
    override val name = ""
    override var prov:provider[Nothing] = myprovider
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
  }

  class sim[A<:dataset[_],B<:model[_,B]](override val initialVal: Double)(implicit override val iterateFrom: dataset[A] => dataset[B],override val tag:ClassTag[B]) extends model[A,B] {
    override var prov:provider[Nothing] = myprovider

    override def apply(initial: Double): dataset[B] = {
      val res = this.initialVal + initial
      class temp extends sim[A,B](initial)
      this.prov.put(this.name,initial)
      (new temp).asInstanceOf[B]
    }
  }

  class recSim[B<:model[_,B],A<:dataset[B]](iterateFrom: dataset[A] => dataset[B],override val initialVal: Double)(implicit override val tag:ClassTag[B]) extends sim[A,B](initialVal)(iterateFrom,tag)
}

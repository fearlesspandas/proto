package Typical.implicits

import Typical.core.Typeable.{build, dataset, model, provider}

import scala.reflect.ClassTag

object implicits {
  implicit class Calc[A <: dataset[_]](a: dataset[A])(implicit tagA:ClassTag[A]){ // f is not being resolved to anything but the identity function
    def calc[U>:A<:model[A,U] with dataset[_]](implicit tagu:ClassTag[U]): dataset[A with U] = {

      val instance = build[U]
      val res = instance.iterateFrom(a)
      instance.apply(res.initialVal).asInstanceOf[dataset[A with U]]
    }
  }
  implicit class DataProvider[A<:dataset[_]](a:dataset[A])(implicit prov:provider[A]) {
    def fetch[U >: A <: dataset[U]](implicit tagu: ClassTag[U]): dataset[U] = {
      val instance = build[U]
      val instance2 = instance.apply(prov.getStateful(instance.name))

      //instance.apply(instance.prov.get[U].collectFirst({case r:Int => r}).get).asInstanceOf[U]
      instance2 //.asInstanceOf[U]
    }
  }
}

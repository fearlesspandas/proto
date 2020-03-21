package Typical.core;

import Typical.core.Typeable
import org.apache.spark.sql.DataFrame

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.collection.mutable
import scala.collection.immutable.HashMap


object Typeable {

  def build[A](implicit tagA:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  def buildName[A](implicit tagA:ClassTag[A]):String = classTag[A].runtimeClass.getSimpleName()
  //Data Accessor/consistency maintainer
  trait provider[-U] {
    //def get[A <: U]: Option[A] //retrieve axioms/calc values (all applicable values should have extended U)
    val mymap:Map[String,Any]
    def get[A](implicit tag:ClassTag[A]):Option[Any]
    def update[A <: dataset[_]](value: A): provider[U]
    def put(s:String,a:Double)
    def getStateful(s:String):Double
  }

  trait Ring[-U] extends sigma[U] {
    def ringplus[A, Ring[A] <: U](a: A, b: A): A
  }
  trait ReRing[U]{
    def reringplus[A>:U](a: A, b: A): A
  }
  trait sigma[-A]

  trait prod[+A]

  trait DataAccess[-A]

  trait DataProduction[+A]

  trait dataset[+A <: dataset[_]]{
    val initialVal:Double
    val name:String
    //type prvdr[U>:A] = provider[U]
    implicit var prov:provider[Nothing]
    def apply(initialVal:Double) : dataset[A] = {
      //val This = this.apply(prov.update[A](initialVal))
      //This.initialVal = initialVal
      //This

      this
    }
//    def apply(p:provider[Nothing]): dataset[A] = {
//      this.prov = p
//      this
//    }
    def setprov(prod:provider[_]) = this.prov = prod
    //def set(value:Int) = this.initialVal = value
    def +[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal + u.initialVal)
    def *[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal * u.initialVal)
  }
  trait ax[A <: ax[A]] extends dataset[A]{
    implicit val tag:ClassTag[A]
    val name:String = classTag[A].runtimeClass.getSimpleName
  }

  trait model[-dependencies <: dataset[_], output <: model[_,output]] extends dataset[output] {//with DataAccess[dependencies with output] with DataProduction[output] {
    //val initialVal: Any
    implicit val tag:ClassTag[output]
    val name:String = classTag[output].runtimeClass.getSimpleName
    implicit val iterateFrom:dataset[dependencies] => dataset[output]
  }

  implicit class Calc[A <: dataset[_]](a: dataset[A])(implicit tagA:ClassTag[A]){ // f is not being resolved to anything but the identity function
    def calc[U>:A<:model[A,U] with dataset[_]](implicit tagu:ClassTag[U]): dataset[A with U] = {

      val instance = build[U]
      val res = instance.iterateFrom(a)
      instance.apply(res.initialVal).asInstanceOf[dataset[A with U]]
    }
  }
  implicit class DataProvider[A<:dataset[_]](a:dataset[A])(implicit prov:provider[A]){
    def fetch[U>:A<:dataset[U]](implicit tagu:ClassTag[U]):dataset[U] = {
      val instance = build[U]
      val instance2 = instance.apply(prov.getStateful(instance.name))

      //instance.apply(instance.prov.get[U].collectFirst({case r:Int => r}).get).asInstanceOf[U]
      instance2//.asInstanceOf[U]
    }
    //    {
//      val initval =
//      class Temp(override val initialVal: Int) extends dataset[U]
//      new Temp(initval).asInstanceOf[U]
//    }//.collectFirst({case r:U => r}).get//data provider must be implicit, and provide access
  }

  //concretely define algebraic operators
  trait number extends ReRing[Int with Double with number] with provider[number] { //with Data[Int with Double with number]
//    override def +[A, Ring[A] <: Int with Double with number](a: A, b: A): A = (a, b) match {
//      case (i: Int, j: Int) => (i + j).asInstanceOf[A]
//      //case (i: Double, j: Double) => (i + j).asInstanceOf[A]
//      case (i:dataset[_],j:dataset[_]) => (i.initialVal + j.initialVal).asInstanceOf[A]
//      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
//    }
    override def reringplus[A>: Int with Double with number](a: A, b: A): A = (a, b) match {
      case (i: Int, j: Int) => (i + j).asInstanceOf[A]
      //case (i: Double, j: Double) => (i + j).asInstanceOf[A]
      case (i:dataset[_],j:dataset[_]) => (i + j).asInstanceOf[A]
      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
    }
    //def +[A>:Int with Double with number](a: A): A = this.+(this.asInstanceOf[A], a)
    val refmap = HashMap[String,Any](("balance",1000),("baserate",1),("TaxBurden",0))
    val statefulmap = mutable.HashMap[String,Any]()
    override val mymap:Map[String,Any] = refmap

    //override def get[A <: number]: Option[A] = ??? //retrieve axioms/calc values (all applicable values should have extended U)
//    def apply(m:HashMap[String,Any]):number= {
//        this.refmap = m
//      this
//    }
    override def get[A](implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = mymap.get(name)
      ret
    }//.collectFirst({case r:Int => r})
    override def update[A <: dataset[_]](value: A): provider[number] = {
      //println("Submitted value " + value.initialVal)
      //this.apply(refmap.updated(buildName[A],build[A].initialVal))
      this
    }
    override def getStateful(s:String):Double = this.statefulmap.get(s).collect({case i:Double => i; case _ => 0}).getOrElse(0)
    override def put(s: String, a: Double): Unit = this.statefulmap.update(s,a)
  }


}

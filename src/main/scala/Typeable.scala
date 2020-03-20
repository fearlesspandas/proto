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
    def update[A <: dataset[_]](value: Any)(implicit tag:ClassTag[A]): provider[U]
  }

  trait Ring[-U] extends sigma[U] {
    def +[A, Ring[A] <: U](a: A, b: A): A
  }
  trait ReRing[U]{
    def +[A>:U](a: A, b: A): A
  }
  trait sigma[-A]

  trait prod[+A]

  trait DataAccess[-A]

  trait DataProduction[+A]

  trait dataset[+A <: dataset[_]]{
    var initialVal:Int
    //type prvdr[U>:A] = provider[U]
    var prov:provider[Nothing]
    def apply(initialVal:Int)(implicit tag:ClassTag[A]) : dataset[A] = {
      val This = this.apply(prov.update[A](initialVal))
      This.initialVal = initialVal
      This
    }
    def apply(p:provider[Nothing]): dataset[A] = {
      this.prov = p
      this
    }
    def setprov(prod:provider[_]) = this.prov = prod
    def set(value:Int) = this.initialVal = value
    def plus[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal + u.initialVal)
  }
  trait ax[A <: ax[A]] extends dataset[A]

  trait model[-dependencies <: dataset[_], +output <: model[_,output]] extends dataset[output] {//with DataAccess[dependencies with output] with DataProduction[output] {
    //val initialVal: Any
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
    def fetch[U>:A<:dataset[U]](implicit tagu:ClassTag[U]):U = {
      val instance = build[U]
      //instance.apply(instance.prov.get[U].collectFirst({case r:Int => r}).get).asInstanceOf[U]
      instance
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
    override def +[A>: Int with Double with number](a: A, b: A): A = (a, b) match {
      case (i: Int, j: Int) => (i + j).asInstanceOf[A]
      //case (i: Double, j: Double) => (i + j).asInstanceOf[A]
      case (i:dataset[_],j:dataset[_]) => (i.plus (j)).asInstanceOf[A]
      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
    }
    def +[A>:Int with Double with number](a: A): A = this.+(this.asInstanceOf[A], a)
    var refmap = HashMap[String,Any](("balance",1000),("baserate",1),("TaxBurden",0))
    override val mymap:Map[String,Any] = refmap

    //override def get[A <: number]: Option[A] = ??? //retrieve axioms/calc values (all applicable values should have extended U)
    def apply(m:HashMap[String,Any]):number= {
        this.refmap = m
      this
    }
    override def get[A](implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = mymap.get(name)
      ret
    }//.collectFirst({case r:Int => r})
    override def update[A <: dataset[_]](value: Any)(implicit tag:ClassTag[A]): provider[number] = {
      //println("Submitted value " + value.initialVal)
      this.apply(refmap.updated(buildName[A],build[A].initialVal))
    }
  }


  case class data[+A<:dataset[_]](override var initialVal: Int = 1) extends dataset[A]{
      override var prov:provider[Nothing] = myprovider
     //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
  }

  class sim[-A<:dataset[_],+B<:model[_,B]](override var initialVal: Int)(implicit override val iterateFrom: dataset[A] => dataset[B]) extends model[A,B] {
    override var prov:provider[Nothing] = myprovider
  }

  class balance extends ax[balance] with number{
    override var  initialVal = 1000
    override var prov:provider[Nothing] = myprovider
  }

  class baserate extends ax[baserate] with number{
    override var  initialVal = 1
    override var prov:provider[Nothing] = myprovider
  }

  //trait three extends ax[three] with number

  //T <: Col[one with two] with Dat[one with two with T[A]]



  implicit object myprovider extends number

  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: dataset[balance with baserate] => dataset[TaxBurden] =
    (src: dataset[balance with baserate]) => {
      //val tax = src.fetch[TaxBurden]
      val bal = src.fetch[balance]
      val rate = src.fetch[baserate]
      val res = bal.plus( rate)
      //println("res " + res)
      new TaxBurden()(res.initialVal)
    }
  class TaxBurden extends sim[balance with baserate, TaxBurden](0) with number{

  }

  implicit val other_itr: dataset[balance with TaxBurden] => dataset[NetIncome] = (src: dataset[balance with TaxBurden]) => {
        val bal = src.fetch[balance]
        val t = src.fetch[TaxBurden]
        val res:number = bal + t
        println("other result " + res)
    new NetIncome()(res.asInstanceOf[Int])
  }

  class NetIncome extends sim[balance with TaxBurden, NetIncome](0) with number

  val result: dataset[balance with baserate with TaxBurden] = data[balance with baserate with TaxBurden]()//.calc[TaxBurden]//.calc[TaxBurden].calc[TaxBurden] //.calc[OtherThing].calc[T]


//  val performanceTest = (0 until 1000).foldLeft(new Col[one with two with T])((a, c) => a.calc[T])
//  (new Col[two with T]).calc[OtherThing]

  def main(args: Array[String]): Unit = {
    val test = result.calc[TaxBurden].calc[TaxBurden].calc[TaxBurden].calc[TaxBurden]
    println("testval " + test.initialVal)
  }

























  //Typesafe definition of a ring
  trait SimpleRing[U]{
    type +[+A<:U,+B<:U] = U //with A with B
    type *[A<:U,B<:U] = U

    //id[A]:A for any A<:U
    //+[A,id]:A
    //defines A as the type within U
    //such that + with any type
    // with identity produces A
    //this value should uniquely be U (up to
    // provable equivalence for any types within
    // the ring
    type idDef[A>:this.+[_,this.idDef[_]]] = A with U
    //def +[A<:U,B<:U](a:A,b:B):this.+[A,B]
    //def *[A<:U,B<:U](a:A,b:B):this.*[A,B]
    class Thing
//    trait thing[A,B<:this.+[A,Id[_]]]{
//      type +[_,B]= A
//    }
    //type thing[A<:U,B<:this.+[A,Id[_]]] = +[A,Id[_]] with A

    type Id[A>:U] = idDef[A] with U

    //
    //type identity[A<:U] = Id[A] with thing[A,+[A,Id[A]]]
  }

  trait RingWithIdentity[U] extends SimpleRing[U]{
    //override type +[A<:U,B>:U] = A
  }

  trait SimpleRingType[U]{
    trait Join[-X]
    trait Meet[+X]
    type With[X,Y] = X with Y
    type +[A<:U,B<:U] = Join[A with B] with U
    type *[A<:U,B<:U] = Meet[A with  B] with U
    //id[A]:A for any A<:U
    //+[A,id]:A
    //defines A as the type within U
    //such that + with any type
    // with identity produces A
    //this value should uniquely be U (up to
    // provable equivalence for any types within
    // the ring
    type idDef[A>:this.+[_,this.idDef[_]]] = A with U
    type Id[A<:U] = idDef[U]
  }
  trait numb


  object ring extends RingWithIdentity[numb]
  import ring._
  //class num[A] extends numb with Id[A]
  //val ring = (new myRing)

  trait AAA extends numb//[AAA]
  trait BBB extends numb//[BBB]
  trait CCC extends numb//[CCC]
  //val testvar:AAA = null.asInstanceOf[+[AAA,BBB]]



}

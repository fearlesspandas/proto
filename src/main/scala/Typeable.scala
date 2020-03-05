package Typical.core;

import Typical.core.Typeable
import org.apache.spark.sql.DataFrame

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.collection.mutable
import scala.collection.immutable.HashMap


object Typeable {

  //Data Accessor/consistency maintainer
  trait provider[-U] {
    def get[A <: U]: Option[A] //retrieve axioms/calc values (all applicable values should have extended U)
    def update[A <: U](value: U): provider[U]
  }

  trait Ring[-U] extends sigma[U] {
    def +[A, Ring[A] <: U](a: A, b: A): A
  }

  trait sigma[-A]

  trait prod[+A]

  trait DataAccess[-A]

  trait DataProduction[+A]

  trait dataset[-A <: dataset[_]] //extends DataAccess[A] with DataProduction[A]
  trait ax[A <: ax[A]] extends dataset[A]

  trait model[dependencies <: dataset[_], output <: model[dependencies,_]] extends dataset[output] with DataAccess[dependencies with output] with DataProduction[output] {
    val initialVal: Any
  }

  implicit class Calc[A <: dataset[_] with DataAccess[_], B <: model[A, _]](a: dataset[A])(implicit f:dataset[A] =>dataset[B]){
    def calc[U>:A]: dataset[A with B] = f(a)
  }
  implicit class Data[A<:dataset[_]](a:dataset[A]){
    def fetch[U>:A<:dataset[U]]:U = ???
  }

  //concretely define algebraic operators
  trait number extends Ring[Int with Double with number] with provider[number] { //with Data[Int with Double with number]
    override def +[A, Ring[A] <: Int with Double with number](a: A, b: A): A = (a, b) match {
      case (i: Int, j: Int) => (i + j).asInstanceOf[A]
      case (i: Double, j: Double) => (i + j).asInstanceOf[A]
      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
    }

    def +[A, Ring[A] <: number](a: A): A = this.+(this.asInstanceOf[A], a)

    override def get[A <: number]: Option[A] = ??? //retrieve axioms/calc values (all applicable values should have extended U)
    override def update[A <: number](value: number): provider[number] = ???
  }


  case class data[A<:dataset[_]]() extends dataset[A]

  class one extends ax[one] with number

  class two extends ax[two] with number

  //trait three extends ax[three] with number

  //T <: Col[one with two] with Dat[one with two with T[A]]
  class T(override val initialVal: Any) extends model[one with two with T, T] with number

  class OtherThing(override val initialVal: Int) extends model[two with T, OtherThing] with number


  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: (dataset[one with two with T] => dataset[T]) =
    (src: dataset[one with two with T]) => {
          val t = src.fetch[T]
      //    val x =  src.data[one].getOrElse(2) .asInstanceOf[number] //+ t//+ src.data[two].get //grab data and use as normal for calculation.
      //    println("is num " + x.isInstanceOf[number]  + t.isInstanceOf[number])
      //    println("option " + x.toString + t.toString)
      //    val result = (x + t)
      //    println("result " + result)
      //    println("is T " + result.isInstanceOf[T])
      new T(0)
    }

  implicit val other_itr: dataset[two with T] => dataset[OtherThing] = (src: dataset[two with T]) => {
    //    val twwo = src.data[two].getOrElse(0).asInstanceOf[number]
    //    val t = src.data[T].getOrElse(0).asInstanceOf[number]
    //    val res = (twwo + t).toInt
    //    println("other result " + res)
    new OtherThing(0)
  }


  //val result = (data[one with two with T with OtherThing])//.calc[T].calc[T].calc[T].calc[OtherThing].calc[T]
//  val performanceTest = (0 until 1000).foldLeft(new Col[one with two with T])((a, c) => a.calc[T])
//  (new Col[two with T]).calc[OtherThing]

//  def main(args: Array[String]): Unit = {
//    p.smap.keys.foreach(s => println(s))
//    (new one).toString()
//    println(result)
//    println(performanceTest)
//  }

























  //Typesafe definition of a ring
  trait SimpleRing[U]{
    type +[A<:U,B<:U] = U //with A with B
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

package Typical.core;

import Typical.core.Typeable.Data
import org.apache.spark.sql.DataFrame

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.collection.mutable
import scala.collection.immutable.HashMap


object Typeable {
  //Data Accessor/consistency maintainer
  trait provider[-U<:Dat[_]]{
    //val myType = typeOf[U]
    //def get[A<:Data[_] with Col[_]](implicit classtag:ClassTag[A]):Option[A]
    //def get[one<:U]:B>:number
    def get[A<:U with Col[_] with Dat[_] with Data[_]](implicit typetag:TypeTag[A], classtag:ClassTag[A]):Option[A] //retrieve axioms/calc values (all applicable values should have extended U)
    //def update[A<:Col[_]](value:U)(implicit classtag: ClassTag[A]):Unit
    def build[A<:Col[_] with Dat[_] with Data[_]](implicit tag:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[Col[A]].asInstanceOf[A]
    def update(key:String,value:Int)
  }
  trait Data[-A<:Col[_]] {
  }

  //algebraic operations
  trait Dat[+U<:Dat[U]]
  {
    def +[A,Dat[A]>:U](a:A,b:A):A
  }
  //Data Type wrapper tied to provider
  //Glues data to scala type layer
  case class Col[A<:Col[_] with Dat[_] with Data[_]](value:Int = 0)(implicit prov:provider[A],ag:TypeTag[A],ctag:ClassTag[A]){
    // if U>:A provider[U]<:provider[A]
    def data[U>:A<: Data[_] with Col[_] with Dat[_]](implicit typetag:TypeTag[U], ctag:ClassTag[U]) : Option[U] = prov.asInstanceOf[provider[U]].get[U] //get but do not update new value in provider
    //def provider[U>:A<:Col[_]]:provider[U] = prov.asInstanceOf[provider[U]]
    val provider = prov
    val refval:Int = (if(value == null) prov.get[A].getOrElse(0).asInstanceOf[Int] else this.value)
   // def run[B](f: A => B): Col[B]
  }
  trait Ax[A<:Ax[A] with Dat[_]] extends Col[A] with Data[A]
  trait Calc[A<:Calc[_,U] with Dat[_],-U<:Col[_]] extends Col[A] with Data[A with U]{
     //def data2[X>:Data[U]<:Data[_]](implicit tag:ClassTag[X]):Option[X] = this.provider.get[X]
  }

  //Grammatical rule that allows for calculation/iteration given data is available
  //and there is a defined way to compute the result
  implicit class Calcable[A<:Col[_] with Dat[_] with Data[_],B<:Calc[_,A] with Dat[_] with Col[B]](a:Col[A])(implicit f: Col[A] => B,prov:provider[B]){
    def calc[U<:B with Dat[_]](implicit tagb: ClassTag[B]):Col[A with B] = {
      val res = f(a)
      prov.update(classTag[B].runtimeClass.getSimpleName(),res.value)
      res.asInstanceOf[Col[A with B]]
    }//provider gets updated upon calculation
  }

  //concretely define algebraic operators
  trait number extends Dat[Int with Double with number] {//with Data[Int with Double with number]
    override def +[A,Dat[A] >: Int with Double with number](a: A, b: A): A = (a,b) match {
      case (i:Int,j:Int) => (i + j).asInstanceOf[A]
      case (i:Double,j:Double) => (i + j).asInstanceOf[A]
      case (i:Col[A],j:Col[A]) => {
        class Temp(override val value:Int)(implicit prov:provider[Temp],ctag:ClassTag[A],ttag:TypeTag[A],tag:ClassTag[Temp],temptag:TypeTag[Temp]) extends Ax[Temp] with number
        (new T(i.value + j.value).asInstanceOf[A with number])
      }
      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
    }
    def +[A,Dat[A] >:number<:Dat[_]](a: A): A = this.+(this.asInstanceOf[A],a)
    def toInt = this match{
      case c:Col[_] => c.value
      case _ => this.asInstanceOf[Int]
    }
    //def +[A<:number](a: A): number = this.+(this.asInstanceOf[A],a)
  }
//  implicit class toNum[A<:number with Col[_]](a:A with number)(implicit tag:TypeTag[A]){
//    //here b would be one <:number, A would be Int
//    def tonum = {
//      a.asInstanceOf[number]
//    }
//  }

  //concretely define data provider object
  //and put it in implicit scope
  implicit object p extends provider[number] {
    trait nummap[B>:Dat[number]] extends Map[String,B]{
      var mymap:Map[String,B] = HashMap[String,B]()
      //val mymap:Map[String] = null
    }

    val nmap = new HashMap[Col[_], number]()
    var smap:HashMap[String,Int] = HashMap[String,Int]()

    smap = smap.updated("one" , 1)
    smap = smap.updated("two",2)
    smap= smap.updated("three", 3)
    smap = smap.updated("T",100)

    //def getter[A <: Col[_]](implicit classtag: ClassTag[A]): Option[A] = smap.get(classTag[A].runtimeClass.getSimpleName()).collectFirst({case a:Int => a.asInstanceOf[A]})
    def getter[A<:number with Col[_] with Dat[_] with Data[_]](implicit ttag:TypeTag[A], classtag: ClassTag[A]): Option[A with number] = {
      println(classTag[A].runtimeClass.getSimpleName())
      class Temp(override val value:Int)(implicit prov:provider[A]) extends Col[A] with number
      //class Temp(override val value:Int)(implicit prov:provider[A]) extends A with number
      Some(
        new T(smap.get(classTag[A].runtimeClass.getSimpleName()).get).asInstanceOf[A with number] //problem
      )
    }
    //def map[A >: number](implicit classTag: TypeTag[A]): Map[String, A] = smap.toMap[String, A]

    override def get[A <: number with Col[_] with Dat[_] with Data[_]](implicit typetag:TypeTag[A],classtag: ClassTag[A]): Option[A with number] = getter[A]

     //override def update[A <: Col[_]](value:number)(implicit classtag: ClassTag[A]): Unit = {smap = smap.updated(classTag[A].runtimeClass.getSimpleName(),value.asInstanceOf[Int])}
    //override def update[A <: Col[_]](value: number)(implicit classtag: ClassTag[A]): Unit = ???

    override def update(key: String, value: Int): Unit = smap = smap.updated(key,value)
  }

//  implicit class pgetter(a:provider[number]){
//    def getter[A>:number<:Col[_]](implicit classtag:ClassTag[A]):Option[A] = a.getter[A]
//  }

  class one extends Ax[one] with number
  class two extends Ax[two] with number

  trait three extends Ax[three] with number
  //T <: Col[one with two] with Dat[one with two with T[A]]
  class T(override val value:Int) extends Calc[T,one with two with T] with number

  class OtherThing(override val value:Int) extends Calc[OtherThing,two with T] with number


  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: (Col[one with two with T] => T) =
    (src:Col[one with two with T]) =>
  {
    val t = src.data[T].getOrElse(0).asInstanceOf[number]
    val x =  src.data[one].getOrElse(2) .asInstanceOf[number] //+ t//+ src.data[two].get //grab data and use as normal for calculation.
    println("is num " + x.isInstanceOf[number]  + t.isInstanceOf[number])
    println("option " + x.toString + t.toString)
    val result = (x + t)
    println("result " + result)
    println("is T " + result.isInstanceOf[T])
    new T(result.toInt)
  }

  implicit val other_itr: Col[two with T] => OtherThing = (src:Col[two with T]) => {
    val twwo = src.data[two].getOrElse(0).asInstanceOf[number]
    val t = src.data[T].getOrElse(0).asInstanceOf[number]
    val res = (twwo + t).toInt
    println("other result " + res)
    new OtherThing(res)
  }


  val result = (new Col[one with two with T]).calc[T].calc[T]
  (new Col[two with T]).calc[OtherThing]
  def main(args:Array[String]):Unit = {
    p.smap.keys.foreach( s => println(s))
    (new one).toString()
    println(result)
  }

//  trait sigma[-U<:sigma[_]]{
//    def f[A>:U]:sigma[A] = ???
//  }
//  class Test extends sigma[Int with String with DataFrame]
//  (new Test).f[DataFrame]
}

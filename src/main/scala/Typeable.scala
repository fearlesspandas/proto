package Typical.core;

import Typical.core.Typeable.Data
import org.apache.spark.sql.DataFrame

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.collection.mutable.HashMap


object Typeable {
  //Data Accessor/consistency maintainer
  trait provider[-U<:Dat[_] ]{
    def get[A<:Data[_] with Col[_]](implicit classtag:ClassTag[A]):Option[A]
    def update[A<:Col[_]](value:U)(implicit classtag: ClassTag[A]):Unit
    def build[A<:Col[_] with Dat[_] with Data[_]](implicit tag:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[Col[A]].asInstanceOf[A]
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
  class Col[A<:Col[_] with Dat[_] with Data[_]](value:A = null.asInstanceOf[A])(implicit prov:provider[A],ag:TypeTag[A],ctag:ClassTag[A]){
    def data[U>:A<: Data[_] with Col[_]](implicit ctag:ClassTag[U]) : Option[U] = prov.get[U] //get but do not update new value in provider
    //def provider[U>:A<:Col[_]]:provider[U] = prov.asInstanceOf[provider[U]]
    val provider = prov
    val refval:A = if(value == null) prov.get[A].get else this.value
   // def run[B](f: A => B): Col[B]
  }
  trait Ax[A<:Ax[A] with Dat[_]] extends Col[A] with Data[A]
  trait Calc[A<:Calc[_,U] with Dat[_],-U<:Col[_]] extends Col[A] with Data[A with U]{
     //def data2[X>:Data[U]<:Data[_]](implicit tag:ClassTag[X]):Option[X] = this.provider.get[X]
  }

  //Grammatical rule that allows for calculation/iteration given data is available
  //and there is a defined way to compute the result
  implicit class Calcable[A<:Col[_] with Dat[_] with Data[_],B<:Calc[_,A] with Dat[_]](a:Col[A])(implicit f: Col[A] => B,prov:provider[B]){
    def calc[U<:B with Dat[_]](implicit tagb: ClassTag[B]):Col[A with B] = {
      val res = f(a)
      prov.update[B](res.asInstanceOf[Col[B]].refval)
      res.asInstanceOf[Col[A with B]]
    }//provider gets updated upon calculation
  }

  //concretely define algebraic operators
  trait number extends Dat[Int with Double with number] {//with Data[Int with Double with number]
    override def +[A,Dat[A] >: Int with Double with number](a: A, b: A): A = (a,b) match {
      case (i:Int,j:Int) => (i + j).asInstanceOf[A]
      case (i:Double,j:Double) => (i + j).asInstanceOf[A]
      case _ => a
    }
    def +[A >:number<:Dat[_]](a: A): A = this.+(this.asInstanceOf[A],a)
  }

  //concretely define data provider object
  //and put it in implicit scope
  implicit object p extends provider[number] {

    val nmap = new HashMap[Col[_], number]()
    val smap = new HashMap[String, number]()

    smap.update("one" , 1.asInstanceOf[number])
    smap.update("two",2.asInstanceOf[number])
    smap.update("three", 3.asInstanceOf[number])

    def getter[A <: Col[_]](implicit classtag: ClassTag[A]): Option[A] = smap.get(classTag[A].runtimeClass.getSimpleName())

    def map[A >: number](implicit classTag: TypeTag[A]): Map[Col[_], A] = nmap.toMap[Col[_], number]

    override def get[A>:number <: Data[_] with Col[_]](implicit classtag: ClassTag[A]): Option[A] = try{
      getter[A]
    }catch{
      case e:Exception => None
    }
     override def update[A <: Col[_]](value:number)(implicit classtag: ClassTag[A]): Unit = smap.update(classTag[A].runtimeClass.getSimpleName(),value)

  }

//  implicit class pgetter(a:provider[number]){
//    def getter[A>:number<:Col[_]](implicit classtag:ClassTag[A]):Option[A] = a.getter[A]
//  }

  case class one() extends Ax[one] with number
  case class two() extends Ax[two] with number

  trait three extends Ax[three] with number
  //T <: Col[one with two] with Dat[one with two with T[A]]
  case class T[A](value:A)(implicit tagA:TypeTag[A]) extends Calc[T[A],one with two with T[A]] with number

  class notanything


  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: (Col[one with two with T[number]] => T[number]) =
    (src:Col[one with two with T[number]]) =>
  {
    val t = src.data[T[number]].get
    val x =  src.data[one].get + t + src.data[two].get //grab data and use as normal for calculation.
    new T[number](x)
  }

  val t = (new Col[one with two with T[number]]).calc[T[number]].calc[T[number]]


//  trait sigma[-U<:sigma[_]]{
//    def f[A>:U]:sigma[A] = ???
//  }
//  class Test extends sigma[Int with String with DataFrame]
//  (new Test).f[DataFrame]
}

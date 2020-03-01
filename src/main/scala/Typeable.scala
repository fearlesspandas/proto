package Typical.core;




import org.apache.spark.sql._
import org.apache.spark.sql.functions._

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._


object Typeable {

  def build[A](implicit c:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  def buildName[A](implicit c:ClassTag[A]) = classTag[A].runtimeClass.getSimpleName()
//  abstract class Ax[A<:Ax[A]](implicit src:DataFrame,taga: ClassTag[A],ttag:TypeTag[A]) extends Col[A]( _ => src.col(classTag[A].runtimeClass.getSimpleName())) {
//    val name = classTag[A].runtimeClass.getSimpleName()
//  }

  trait provider[+U<:Dat[U]]{
    def get[A<: Data[A]]:A//(implicit src:U)
//    def +[A>:U,B>:U](a:A,b:B):U
//    def +++[A>:U](a:A): A
  }
  trait Data[-A] {
  }
  trait Dat[+U<:Dat[U]]
  {
    def get[A<: Data[A]]:A//(implicit src:U)
    def +[A>:U](a:A,b:A):A
    //def +++[A>:U](a:A): A
  }
  class Col[A<:Col[_]](implicit tag:TypeTag[A],ctag:ClassTag[A]){
    def data[U >: A<:Data[U]](implicit tag:ClassTag[U]) : U = classTag[U].runtimeClass.newInstance().asInstanceOf[U]
   // def run[B](f: A => B): Col[B]
  }
  abstract class Calc[A<:Calc[_,U],-U<:Col[_]](implicit tagA:TypeTag[A],tagU:TypeTag[U],ctagA:ClassTag[A]) extends Col[A] with Data[U]{
    //val func = this.f
  }
  implicit class Thing[A<:Data[A]](a:A)(implicit data:provider[_]){
      def get():A = data.get[A]
      //def +[U>:A,B>:U](u:U,b:B): A = data.+[U,B](a,b)
  }
  implicit class other[A<:Col[_],B<:Col[_]](a:A)(implicit f: A => B,tagB:ClassTag[B]){
    def produce[U>:B]():U = f(a)
  }
  implicit class Calcable[A<:Col[_],B<:Calc[_,A]](a:Col[A])(implicit f: Col[A] => B){
    def calc[U>:B]():B = null.asInstanceOf[B]
  }


  //traits -> datatypes
  trait number extends Dat[Int with Double with number]{
    override def get[A <: Data[A]]: A = ???
    override def +[A >: Int with Double with number](a: A, b: A): A = ???
  }

  class one extends Col[one] with number
  class two extends Col[two] with number
  class three extends Col[three]
  class T extends Calc[T,one with two] with number

  class p(src:String) extends provider[number]{
    override def get[A <: Data[A]]: A = ???
    //override def +(a:int,b:int):int = ???
    //override def +[A >: number, B >: number](a: A, b: B): number = ???

    ///override def +++[A >: number](a: A): A = ???
  }
  implicit val numberprovider = new p("")

  implicit val f: (Col[one with two ] => T) =
    (src:Col[one with two]) =>
  {
    src.data[one].get[one].+[Double](0,0)
  }
  (new Col[one with two]).calc[T]
}
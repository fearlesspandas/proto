package Typical.core;

import Typical.core.Typeable
import org.apache.spark.sql.DataFrame

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.collection.mutable
import scala.collection.immutable.HashMap
import Typical.implicits.implicits._

object Typeable {
  def build[A](implicit tagA:ClassTag[A]):A = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
  def buildName[A](implicit tagA:ClassTag[A]):String = classTag[A].runtimeClass.getSimpleName()

  //Data Accessor/consistency maintainer
  trait provider[-U] {
    val mymap:Map[String,Any]
    def get[A](implicit tag:ClassTag[A]):Option[Any]

    def put(s:String,a:Any)
    def getStateful(s:String):Any
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
    val initialVal:Any
    val name:String
    implicit var prov:provider[Nothing]
    def apply(initialVal:Any) : dataset[A] = this
    def setprov(prod:provider[_]) = this.prov = prod
    //def set(value:Int) = this.initialVal = value
//    def +[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal + u.initialVal)
//    def -[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal - u.initialVal)
//    def *[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal * u.initialVal)
//    def /[U<:dataset[_]](u:U):dataset[A] = this.apply(this.initialVal / u.initialVal)
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

  //concretely define algebraic operators
  trait number extends ReRing[Int with Double with number] with provider[number] { //with Data[Int with Double with number]
    override def reringplus[A>: Int with Double with number](a: A, b: A): A = (a, b) match {
      case (i: Int, j: Int) => (i + j).asInstanceOf[A]
      //case (i: Double, j: Double) => (i + j).asInstanceOf[A]
      //case (i:dataset[_],j:dataset[_]) => (i + j).asInstanceOf[A]
      case _ => (a.asInstanceOf[Double] + b.asInstanceOf[Double]).asInstanceOf[A]
    }
    //def +[A>:Int with Double with number](a: A): A = this.+(this.asInstanceOf[A], a)
    val refmap = HashMap[String,Any](("balance",1000),("baserate",1),("TaxBurden",0))
    val statefulmap = mutable.HashMap[String,Any]()
    override val mymap:Map[String,Any] = refmap

    override def get[A](implicit tag:ClassTag[A]):Option[Any] = {
      val name = buildName[A]
      val ret = mymap.get(name)
      ret
    }
    override def getStateful(s:String):Any = this.statefulmap.get(s).collect({case i:Double => i; case s:Seq[_] => s; case _ => null}).getOrElse(null)
    override def put(s: String, a: Any): Unit = this.statefulmap.update(s,a)
  }


}

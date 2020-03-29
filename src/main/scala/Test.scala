import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
object Test {

    class A extends axiom[Double,A](5d)
    //example of a recursive numeric sequence that enforces convergence
    val X_f = (src:dataset[X with A with XConverges with XSum]) =>{
      //println("x func")
      val x = src.fetchDouble[X]
      val a = src.fetchDouble[A]
      val converges = src.calc[Boolean,XConverges]
      //val res: dataset[X] with InitialType[Double, X] = if (!converges.typedInitVal) ((x*x) + a)/(x*2) else x
      val res: dataset[X] with InitialType[Double, X] = converges.typedInitVal match {
        //case true =>  x
        case _ => (x/2).asInstanceOf[dataset[X] with InitialType[Double,X]]//(((x*x) + a)/(x*2))
      }
      res

      //x + a
    }
  val X_func = X_f.set[X]

  val Y_f = (src:dataset[Y with A]) =>{
    //println("x func")
    val x = src.fetchDouble[Y]
    val a = src.fetchDouble[A]
    //val res: dataset[X] with InitialType[Double, X] = if (!converges.typedInitVal) ((x*x) + a)/(x*2) else x
    //case true =>  x
    (((x*x) + a)/(x*2))


    //x + a
  }
  val Y_func = Y_f.set[Y]

  class Y extends recSim[Double,Y,Y with A](Y_func)(1d)

  class X extends recSim[Double,X,X with A with XConverges with XSum](X_func)(1d)
  implicit val ctx = myprovider.register[A].register[X].register[XConverges].register[XSum].register[Y].register[YConverges]
  class XConverges extends SeqLooksConvergent[XConverges,A with X with XConverges with XSum,X,XSum](0.000002d,10)
  class YConverges extends LooksConvergent[YConverges,A with Y,Y](.02d,10)
  class XSum extends sum[XSum,X with A with XConverges with XSum,X]
  val k = 10
  lazy val s = Seq((0 until 1).map(_ => (0 until k).foldLeft[dataset[A with X with XConverges with XSum with Y with YConverges]](data[A with X with XConverges with XSum with Y with YConverges](ctx))( (a,c) => a.calc[Double,Y].calc[Double,X])):_*)//.par

  def main(args: Array[String]): Unit = {
    val t = k + 1
    val t0 = System.nanoTime()
    println(s"Starting Test with $k iterations")
    //println(s.map(d => d.initialVal))
    println(s"Xconverges:${s.map(d => d.calc[Boolean,XConverges].initialVal)}")
    println(s"YConverges:${s.map(d => d.calc[Boolean,YConverges].initialVal)}")
    println(s"X:${s.map(d => d.fetchDouble[X].initialVal)}")
    println(s"Y:${s.map(d => d.fetchDouble[Y].initialVal)}")
   // println(s"X after $k iterations " + performanceTest.fetchDouble[X].initialVal)
    //println(s"Sum of X :" + performanceTest.fetchDouble[XSum].initialVal)
    //println(s"Does X converge ? " + performanceTest.calc[Boolean,XConverges].initialVal)

    //    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
//    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
object Test {

  /**
   * This is an example of a simple custom simulation built to model tax burden over N years.
   * It requires two axioms as dependencies, taxable balance, and the base tax rate. These are
   * effectively constants. We can start by extending the convenient axiom class and providing an
   * initial value
   *
   */


    class A extends axiom[Double,A](5d)
    class eps extends axiom[Double,eps](.0003)

    val X_f = (src:dataset[X with A]) =>{
      val x = src.fetchDouble[X]
      val a = src.fetchDouble[A]
      ((x*x) + a)/(x*2)
      //x + x
    }
  implicit val X_func = X_f.set[X]

  class X extends recSim[Double,X,X with A](X_func)(1d)

  val converges = (src:dataset[ eps with X with A]) => {
    val epsilon = src.fetchDouble[eps]
    val x = src.fetchDouble[X]
    val expected = scala.math.sqrt(src.fetchDouble[A].typedInitVal)
    scala.math.abs(x.typedInitVal - expected) < epsilon.typedInitVal
  }
  implicit val converges_f = converges.set[doesConverge]
  class doesConverge extends sim[Boolean,eps with X with A,doesConverge](false)


    val n = 5
  val ctx = myprovider.register[A].register[X].register[eps].register[doesConverge]
  lazy val performanceTest = (0 until n).foldLeft[dataset[A with X with eps with doesConverge]](
    data[A with X with eps with doesConverge](ctx)
    )( (a, c) => a.calc[Double,X])
//  lazy val performanceTest2 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](
//    data[balance with baserate with TaxBurden with mylist](ctx)
//  )( (a, c) => a.calc[Double,TaxBurden])
//
//
//  lazy val performanceTest3 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](
//    data[balance with baserate with TaxBurden with mylist](performanceTest.dataprovider())
//  )( (a, c) => a.calc[Double,TaxBurden])

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println("Starting performance test")
    println(s"X after $n iterations " + performanceTest.initialVal)
    println(s"Does X converge :" + performanceTest.calc[Boolean,doesConverge].initialVal)
//    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
//    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
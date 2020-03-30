import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
object Test {

  /**
   * Here we use Typical to build a simple prover of Cauchy convergence https://en.wikipedia.org/wiki/Cauchy%27s_convergence_test
   * It takes an epsilon double and an integer for setting max iterations of verifiability.
   *
   *
   * Begin by setting any contextual variables as axioms.
   */
  class A extends axiom[Double,A](5d)
  /**
   * Define our sequences with our classes that encapsulate them
   */
  val X_f = (src:dataset[X]) =>{
      val x = src.fetchDouble[X]
      x/2
    }
  val X_func = X_f.set[X]
  class X extends recSim[Double,X,X](X_func)(1d)
  val Y_f = (src:dataset[Y with A]) => {
    val x = src.fetchDouble[Y]
    val a = src.fetchDouble[A]
    ((x*x) + a)/(x*2)
  }
  val Y_func = Y_f.set[Y]
  class Y extends recSim[Double,Y,Y with A](Y_func)(1d)
  /**
   * We then define our convergence provers. YConverges proves type Y with dependencies A with Y varies at most by eps over the next N iterations
   * In this particular example we expect Y to converge to sqrt(A)
   */
  class YConverges extends LooksConvergent[YConverges,A with Y,Y](.02d,10)

  /**
   * Sum type of X with dependencies X. Note, sum is a thin wrapper around recursive sim
   * and must be iterated to be updated. Meaning for example the values for
   * dataset[A with B].calc[A].calc[A].calc[sum[A]] != dataset[A].calc[A].calc[sum[A]].calc[A].calc[sum[A]]
   * LooksConvergent types will iterate any target type you can pass them including sum,
   * which gives us all we need to test cauchy convergence, given the above definition for LooksConvergent
   */
  class XSum extends sum[XSum,X,X]
  /**
   * Define Cauchy Convergence test for XSum. sums are recursive sim types and therefore need to be
   * included in their own dependencies
   */
  class XSumConverges extends LooksConvergent[XSumConverges, X  with XSum,XSum](.000002d,10)
  /**
   * Prepare the context provider with the initial values
   */
  implicit val ctx = myprovider.register[A].register[X].register[XSum].register[Y].register[YConverges].register[XSumConverges]
  /**
   * Now we are all set to build our simulation and run it. Here we're wrapping many sims in sequence for experimentation with parrallel processing of sims.
   *
   * We include our above classes as dependencies, and iterate X and Y k times before testing convergence.
   * Modifying k and m values will reveal how X converges very slowly compared to Y
   */
  val k = 50              //number of iterations per sim
  val m = 1                   //number of sims we want to run
  lazy val s = Seq((0 until m).map(_ => (0 until k).foldLeft[dataset[A with X  with XSum with Y with YConverges with XSumConverges]](data[A with X  with XSum with Y with YConverges with XSumConverges](ctx))( (a,c) => a.calc[Double,X].calc[Double,Y])):_*)//.par
  val test: dataset[X with XSum with Y] with InitialType[_,_] = data[X with XSum with Y](ctx)
  val t: dataset[XSum with X with Y] with InitialType[Double, XSum with X with Y] = test.calc[Double,X]//.calc[Double,Y]
  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println(s"Starting Test with $k iterations")
    println(s"X Is Cauchy:${s.map(d => d.calc[Boolean,XSumConverges].initialVal)}")
    println(s"YConverges:${s.map(d => d.calc[Boolean,YConverges].initialVal)}")
    println(s"X:${s.map(d => d.fetchDouble[X].initialVal)}")
    println(s"Y:${s.map(d => d.fetchDouble[Y].initialVal)}")
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
import Typical.core.Typeable._
import Typical.impl._
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
   * Define our sequences as functions from dataset containing our dependencies.
   * Later when using the calc method to iterate our functions, calc will take
   * this function, if it's the appropriate type, and use it to construct a functor
   * for a new dataset with the updated value.
   *
   */
  val X_f = (src:dataset[X]) =>{
      val x = src.fetchDouble[X]
      //this calculation is typechecked based on the initial type of X
      x/2
    }
  val X_func = X_f.set[X]

  /**
   * Bind it all to a recursive simulation type, which takes datatype<:Any,self<:self,dependencies<:dataset[_]
   * as type paramaters
   */
  class X extends recSim[Double,X,X](X_func)(1d)
  val Y_f = (src:dataset[Y with A]) => {
    //in the future these calls to fetch src can hopefully be made implicit
    val x = src.fetchDouble[Y]
    val a = src.fetchDouble[A]
    println("Y state:" + src.fetchFromState[Double,Y](15).typedInitVal)
    //all operations are typechecked to ensure all datasets have correct initial types
    ((x*x) + a)/(x*2)
  }
  val Y_func = Y_f.set[Y]
  class Y extends recSim[Double,Y,Y with A](Y_func)(1d)
  /**
   * We then define our convergence provers. YConverges if true proves type
   * Y with dependencies A with Y varies at most by eps over the next N iterations
   * In this particular example we expect Y to converge to sqrt(A)
   */
  class YConverges extends LooksConvergent[YConverges,A with Y,Y](.02d,10)

  /**
   * Next we build sum types of X with dependencies X. Note, sum is a thin wrapper around bind which is itself a thin
   * wrapper around a recursive sim and must be iterated by calc to be updated. Meaning by default the values for
   * dataset[A with B].calc[A].calc[A].calc[sum[A]] != dataset[A with B].calc[A].calc[sum[A]].calc[A].calc[sum[A]]
   *
   * LooksConvergent types, another thin wrapper around recursive sim, will iterate any target type you can pass them ,including sum, within
   * their calculation function. This is all we need for a naive test of Cauchy convergence, given the above definition for LooksConvergent.
   *
   * We also illustrate how to conveniently trigger updates to sums value automatically using bindings. This feature
   * currently only supports bindings in limited context, but serves as a good example to the motivation. Note
   * that sums are themselves of type bind.
   *
   */
  class XSum extends sum[XSum,X,X]
  class othersum extends sum[othersum,X,X]
  /**
   * another example with more dependencies on the target type
   */
  class ysum extends sum[ysum,Y with A, Y]
  /**
   * Define Cauchy Convergence test for XSum (convergence of the partial sums of the series produced by X). sums are recursive sim types and therefore need to be
   * included in their own dependencies
   */
  class XSumConverges extends LooksConvergent[XSumConverges, X  with XSum,XSum](.000002d,10)
  /**
   * Prepare the context provider with the initial values
   */
  implicit val ctx = myprovider.register[A].register[X].register[XSum].register[Y].register[YConverges].register[XSumConverges].register[othersum].register[ysum]
  /**
   * Now we are all set to build our simulation and run it. Here we're wrapping many sims in sequence for easy experimentation with parrallel processing of sims.
   *
   * We include our above classes as dependencies, and iterate X and Y k times before testing convergence.
   * Modifying k and m values will reveal how X converges very slowly compared to Y
   */
  val k = 3              //number of iterations per sim
  val m = 1                   //number of sims we want to run
  lazy val s = Seq((0 until m).map(_ => (0 until k).foldLeft(
    data[
      A with X  with XSum with Y with YConverges with XSumConverges with othersum with ysum
    ](ctx).dataset
  )( (a,_) => a.calcWithBinding[Double,X].calc[Double,Y].calc[Double,othersum])):_*)//.par

  /**
   * Here we are using calcWithBinding to calculate X. That means if there is a binding in the dataset
   * and it can be applied to X, then the full binded calculation reduces to a call of calc on X with a following call on
   * the bind type. Recall that sum types are bindings, hence in our example, when the code below is executed, you
   * will see the final result of XSum and othersum being equal, in spite of the fact that we never directly called calc on XSum.
   *
   */

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println(s"Starting Test with $k iterations")
    println(s"X Is Cauchy:${s.map(d => d.calc[Boolean,XSumConverges].initialVal)}")
    println(s"YConverges:${s.map(d => d.calc[Boolean,YConverges].initialVal)}")
    println(s"X:${s.map(d => d.fetchDouble[X].initialVal)}")
    println(s"Y:${s.map(d => d.fetchDouble[Y].initialVal)}")
    println(s"XSum: ${s.map(d => d.fetchDouble[XSum].initialVal)}")
    println(s"othersum: ${s.map(d => d.fetchDouble[othersum].initialVal)}")
    println(s"statesequence: ${s.map(d => d.dataprovider().statestore)}")
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
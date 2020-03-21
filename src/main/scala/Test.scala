import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag

object Test {

  /**
   * This class is an example of a simple custom simulation built to model tax burden over N years.
   * It requires two axioms as dependencies, taxable balance, and the base tax rate. These are
   * effectively constants. We can start but extending the convenient axiom class and providing an
   * initial value
   *
   */

  class balance extends axiom[balance](1000d) with number

  class baserate extends axiom[baserate](0.23) with number


  /**
   * The TaxBurden class then defines a recursive sim. recSim is structured as recSim[output,dependencies] and in the case of a recursive
   * sim, it must itself be included as a dependency (otherwise a compile time error will be thrown). Currently in recursive sims (but not
   * in non-recursive sims) the implicit value must be passed explicitly in the constructor.
   */

  class TaxBurden extends recSim[TaxBurden,balance with baserate with TaxBurden](iterateFrom = T_Iterator)(0d ) with number

  /**
   * We then concretely define what we want our recursive calculation to be as a generic function
   * between datasets. Typical.core.dataset is the core structure behind all of Typicals processing.
   * It allows us to safely access and update data in a type safe way.
   * IMPORTANT NOTE: If data is somehow accessed outside of the src dataset api, type safe processing
   * is not guarenteed.
   */
  implicit val T_Iterator: dataset[balance with baserate with TaxBurden] => dataset[TaxBurden] =
    (src: dataset[balance with baserate with TaxBurden]) => {
      /**
       * Retrieve the data we need for our calculation using dataset.fetch
       */
      val tax = src.fetch[TaxBurden]
      val bal = src.fetch[balance]
      val rate = src.fetch[baserate]

      /**
       * Use any operations defined for datasets which include common algebraic operations
       */
      val res = (bal * rate) + tax
      new TaxBurden()(res.initialVal)
    }

  /**
   * Example of a non-recursive sim. Simply retrieves tax burden and copies it.
   */
  implicit val tester_map = (src:dataset[TaxBurden]) => {
    val tx = src.fetch[TaxBurden]
    new TaxTester()(tx.initialVal)
  }

  /**
   * If our calculating function is in the implicit scope we don't have to declare it explicitly
   * DevNote: There should not be a required initial value for non recursive sims.
   */
  class TaxTester extends sim[TaxBurden,TaxTester](0d)


  /**
   * running a sim is done with the convenient data class, which implements dataset.
   * First we build a dataset as follows
   */
  val dat: dataset[balance with baserate with TaxBurden with TaxTester] = data[balance with baserate with TaxBurden with TaxTester](0)//.calc[TaxBurden]//.calc[TaxBurden].calc[TaxBurden] //.calc[OtherThing].calc[T]
  /**
   * Any data type defined above that has a valid calculating function in scope, relative to this dataset, will be available for
   * immediate calculation through the calc method. For example, if we removed balance from dat, we would not be able to calculate TaxBurden,
   * because balance is dependency for TaxBurden.
   *
   * For non-recursive sims, calc is safe to call in any context, as it is essentially non-stateful.
   * The syntax for calculating is as follows
   */
  dat.calc[TaxTester].initialVal

  /**
   * With recursive sims theres a few important things to remember when building more complex simulations.
   * First is that calc returns a dataset of the same type as the one you called it on (it is monadic in this regard)
   * and therefore many recursive, or non recursive sims can be chained together so long as they can be computed from the
   * initial dataset
   * Second  is that ORDER MATTERS when building up a sim with calc. Doing dataset[A with B].calc[A].calc[B] will not
   * yield the same result as dataset[A with B].calc[B].calc[A] if either A or B depend on one another and are recursive.
   * calc inherrently allows for some amount of statefullness using this mechanism. The sequence of calc calls that
   * are done to build up a sim essentially uniquely define that simulation.
   *
   * As a simple example we iterate our TaxBurden over some number n of years using a fold left.
   * If we wanted to bring our sim only to a state of only having calculated n/2 years, for example,
   * we simply just stop calling calc after n/2 iterations on TaxBurden
   */
  val performanceTest = (0 until 100000).foldLeft[dataset[balance with baserate with TaxBurden]](data[balance with baserate with TaxBurden](0))((a, c) => a.calc[TaxBurden]).initialVal

  def main(args: Array[String]): Unit = {
    println("Total Tax burden over 1000000 years " + performanceTest)
  }
}
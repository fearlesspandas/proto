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


  class balance extends axiom[Double,balance](1000d)

  class baserate extends axiom[Double,baserate](0.23)

  implicit val t : dataset[balance] => dataset[mylist] = _ => new mylist()
  class mylist extends sim[Seq[_],balance,mylist](Seq(12d,3d,4d,5d))

  /**
   * The TaxBurden class then defines a recursive sim. recSim is structured as recSim[output,dependencies] and in the case of a recursive
   * sim, it must itself be included as a dependency (otherwise a compile time error will be thrown). Currently in recursive sims (but not
   * in non-recursive sims) the implicit value must be passed explicitly in the constructor.
   */
  //implicit val p:provider[_] = m3
  class TaxBurden extends recSim[Double,TaxBurden,balance with baserate with TaxBurden with mylist](T_Iterator)(1d)

  /**
   * We then concretely define what we want our recursive calculation to be as a generic function
   * between datasets. Typical.core.dataset is the core structure behind all of Typicals processing.
   * It allows us to safely access and update data in a type safe way.
   * IMPORTANT NOTE: If data is somehow accessed outside of the src dataset api, type safe processing
   * is not guarenteed.
   */
   val Iterator = (src: dataset[balance with baserate with mylist with TaxBurden]) => {
      /**
       * Retrieve the data we need for our calculation using dataset.fetch
       */
      val tax = src.fetch[Double,TaxBurden]
      val initval = tax.typedInitVal
      val bal = src.fetchDouble[balance]
      val rate = src.fetchDouble[baserate]
      /**
       * an example of how to retrieve nonnumeric data. As dataset with an initial type of sequence
       * we have different available operations to use in context
       */
      val l = src.fetchSeq[mylist]


      println(s"Calling T iterator bal:$bal,rate:$rate,tax:$initval")
      /**
       * Use any operations defined for datasets with respective initial value types which include common algebraic operations
       */
      ((bal * rate)) + tax

    }

    implicit val T_Iterator = Iterator.set[TaxBurden]
  /**
   * Example of a non-recursive sim. Simply retrieves tax burden and copies it.
   */
  implicit val tester_map = (src:dataset[TaxBurden with balance]) => {
    val tx = src.fetchDouble[TaxBurden]
    val bal = src.fetchDouble[balance]
    tx
  }
  implicit val TaxTestermap = tester_map.set[TaxTester]

  /**
   * If our calculating function is in the implicit scope we don't have to declare it explicitly
   * DevNote: There should not be a required initial value for non recursive sims.
   */
  class TaxTester extends sim[Double,TaxBurden with balance,TaxTester](0d)


  /**
   * running a sim is done with the convenient data class, which implements dataset.
   * First we build a dataset as follows
   */
  //val dat: dataset[balance with baserate with TaxBurden with TaxTester] = data[balance with baserate with TaxBurden with TaxTester](0)//.calc[TaxBurden]//.calc[TaxBurden].calc[TaxBurden] //.calc[OtherThing].calc[T]
  /**
   * Any data type defined above that has a valid calculating function in scope, relative to this dataset, will be available for
   * immediate calculation through the calc method. For example, if we removed balance from dat, we would not be able to calculate TaxBurden,
   * because balance is dependency for TaxBurden.
   *
   * For non-recursive sims, calc is safe to call in any context, as it is essentially non-stateful.
   * The syntax for calculating is as follows
   */
  //dat.calc[TaxTester].initialVal

  /**
   * With recursive sims theres a few important things to remember when building more complex simulations.
   * First is that calc returns a dataset of the same type as the one you called it on (it is somewhat monadic in this regard)
   * and therefore many recursive, or non recursive sims can be chained together so long as they can be computed from the
   * initial dataset
   * Second  is that ORDER MATTERS when building up a sim with calc. Doing dataset[A with B].calc[A].calc[B] will not
   * yield the same result as dataset[A with B].calc[B].calc[A] if either A or B depend on one another and are recursive.
   * calc inherrently allows for some amount of statefullness using this mechanism. The sequence of calc calls that
   * are done to build up a sim essentially uniquely define that simulation.
   *
   * Statefulness can be fully encapsulated using contexts, which are just implementations of providers.
   * providers are the general type used to encapsulate data persistence. In this implementation it is
   * just a thin wrapper around a hashmap. providers are functionally passed forward over iterations of the
   * calc method. i.e. val a = data(ctx)[A].calc[A]; val b = data(ctx)[A].calc[A].calc[A] have different states
   * if type A is updated during calculations (this is of most concern when dealing with recursive sims type theoretically)
   *
   * This statefulness can be seen in the below example, where both performanceTest and performanceTest2
   * return the same result. If they shared state, i.e. if tax burden was iterated more for one sim run
   * than the other, we would have differing results.
   *
   * As a simple example we iterate our TaxBurden over some number n of years using a fold left.
   * If we wanted to bring our sim only to a state of only having calculated n/2 years, for example,
   * we simply just stop calling calc after n/2 iterations on TaxBurden
   *
   *
   */
    val n = 10
  val ctx = myprovider.register[balance].register[baserate].register[TaxBurden]
  lazy val performanceTest = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](data[balance with baserate with TaxBurden with mylist](ctx))( (a, c) => a.calc[Double,TaxBurden])
  lazy val performanceTest2 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](data[balance with baserate with TaxBurden with mylist](ctx))( (a, c) => a.calc[Double,TaxBurden])


  /**
   * If we wanted them to share state we could simply continue iterating calc from the output state provider (dataset.dataprovider()) as our initial context.
   * This is shown in the below example, where we can see our sim starts off contextually exactly where performancetest left off
   */

  lazy val performanceTest3 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](data[balance with baserate with TaxBurden with mylist](performanceTest.dataprovider()))( (a, c) => a.calc[Double,TaxBurden])

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println("Starting performance test")
    println(s"Total Tax burden over $n years P1 " + performanceTest.initialVal)
    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
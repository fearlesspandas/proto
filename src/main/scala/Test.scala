import Typical.core.Typeable._
import Typical.impl._

import Typical.implicits.implicits._
object Test {

  /**
   *
   * Here we use typical to illustrate a very straightforward
   * implementation of the general KnapSack problem.
   *
   * If solving the bounded KnapSack problem where the number of
   * items is at most k>0, the calc method should be called k
   * times on the KnapSack type. This is because, in the below
   * implementation, we use typical to build up the functional
   * structure recursively. We then submit a desired weight
   * to function that is the output of the KnapSack calculation
   *
   * To Solve the unbounded KnapSack problem, simply assume
   * the above k is equal to the number of items in the Items
   * axiom.
   *
   */
  case class item(weight:Double,value:Double)
  class Items extends axiom[Seq[item],Items](Seq(
    item(10,10),
    item(5,10),
    item(94d,1000),
    item(1,1),
    item(1,1),
    item(1,1),
    item(1,1),
    item(1,9)
  ))
  class KnapSack extends recSim[Double => Double,KnapSack,KnapSack with Items](
    //Additional resources explaining unbounded knap-sack
    //https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithmj
    ((src:dataset[KnapSack with Items]) => {
      val prevMap = src.fetch[Double => Double,KnapSack].typedInitVal
      val items = src.fetch[Seq[item],Items].typedInitVal
      (w:Double) => {
        items.map(i => if(w - i.weight > 0) i.value + prevMap(w - i.weight) else 0d).sorted.reverse.head
      }
  }).set[KnapSack])(_ => 0)


  val dat = data[KnapSack with Items](myprovider.register[KnapSack].register[Items])

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    //Recall the number of calc iterations determines the max number of items
    //Here we are optimizing for up to 2 items at most
    val knapsackFunc = dat
      .calc[Double => Double,KnapSack]
      .calc[Double => Double,KnapSack]
      //add another calc[Double => Double,KnapSack] here
      //to see how 3 items will change the optimum
      .typedInitVal
    //recall the output of the KnapSack type
    //is a a function that will solve
    //for two items under a provided weight
    val maxWeight = 100d
    val res = knapsackFunc(maxWeight)
    println(s"Weight = $maxWeight Highest Possible value = $res")
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
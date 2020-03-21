import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag

object Test {

  class balance extends axiom[balance](1000) with number

  class baserate extends axiom[baserate](1) with number


  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: dataset[balance with baserate with TaxBurden] => dataset[TaxBurden] =
    (src: dataset[balance with baserate with TaxBurden]) => {
      val tax = src.fetch[TaxBurden]
      val bal = src.fetch[balance]
      val rate = src.fetch[baserate]
      val res = (bal * rate) + tax
      new TaxBurden()(res.initialVal)
    }
  class TaxBurden extends recSim[TaxBurden,balance with baserate with TaxBurden](T_Iterator,1) with number

  implicit val x = (src:dataset[TaxBurden]) => {
    val tx = src.fetch[TaxBurden]
    new TaxTester()(tx.initialVal)
  }
  class TaxTester extends sim[TaxBurden,TaxTester](0)

  val dat: dataset[balance with baserate with TaxBurden with TaxTester] = data[balance with baserate with TaxBurden with TaxTester](0)//.calc[TaxBurden]//.calc[TaxBurden].calc[TaxBurden] //.calc[OtherThing].calc[T]


    val performanceTest = (0 until 100000).foldLeft[dataset[balance with baserate with TaxBurden]](data[balance with baserate with TaxBurden](0))((a, c) => a.calc[TaxBurden])
  //  (new Col[two with T]).calc[OtherThing]

  def main(args: Array[String]): Unit = {
    //val test = result.calc[TaxTester].calc[TaxBurden].calc[TaxTester].calc[TaxBurden].calc[TaxTester].calc[TaxBurden].calc[TaxBurden]
    println("Total Tax burden over 1000000 years " + performanceTest.initialVal)
  }
}
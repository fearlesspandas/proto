import Typical.core.Typeable._

import scala.reflect.ClassTag

object Test {


  //trait three extends ax[three] with number

  //T <: Col[one with two] with Dat[one with two with T[A]]



  implicit object myprovider extends number


  case class axiom[A<:ax[A]](override val initialVal: Int)(override implicit val tag:ClassTag[A],override implicit var prov:provider[Nothing] ) extends ax[A]{
    //override var prov:provider[Nothing] = myprovider
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
    override def apply(initial: Int): dataset[A] = {
      val res = this.initialVal + initial
      class temp extends axiom[A](initial)
      (new temp).asInstanceOf[A]
    }
  }


  class balance extends axiom[balance](1000) with number{
    //override var prov:provider[Nothing] = myprovider
  }

  class baserate extends axiom[baserate](1) with number{
    //override var prov:provider[Nothing] = myprovider
  }


  case class data[+A<:dataset[_]](override val initialVal: Int) extends dataset[A]{
    override val name = ""
    override var prov:provider[Nothing] = myprovider
    //def apply(): dataset[A] = new data().asInstanceOf[dataset[A]]
  }

  class sim[A<:dataset[_],B<:model[_,B]](override val initialVal: Int)(implicit override val iterateFrom: dataset[A] => dataset[B],override val tag:ClassTag[B]) extends model[A,B] {
    override var prov:provider[Nothing] = myprovider

    override def apply(initial: Int): dataset[B] = {
      val res = this.initialVal + initial
      class temp extends sim[A,B](initial)
      this.prov.put(this.name,initial)
      (new temp).asInstanceOf[B]
    }
  }

  //need Col[one with two] => number to a subtype of Col[one with two] => T[Double]
  implicit val T_Iterator: dataset[balance with baserate] => dataset[TaxBurden] =
    (src: dataset[balance with baserate]) => {
      //val tax = src.fetch[TaxBurden]
      val bal = src.fetch[balance]
      println("balance " + bal)
      val rate = src.fetch[baserate]
      println("rate " + rate )
      val res = bal.plus( rate)
      println("res " + res)
      new TaxBurden()(res.initialVal)
    }
  class TaxBurden extends sim[balance with baserate, TaxBurden](0) with number

  implicit val x = (src:dataset[TaxBurden]) => {
    val tx = src.fetch[TaxBurden]
    println("TaxBurden Test: " + tx.initialVal)
    new TaxTester()(tx.initialVal)
  }
  class TaxTester extends sim[TaxBurden,TaxTester](0)

  implicit val other_itr: dataset[balance with TaxBurden] => dataset[NetIncome] = (src: dataset[balance with TaxBurden]) => {
    val bal = src.fetch[balance]
    val t = src.fetch[TaxBurden]
    //val res:number = bal + t
    //println("other result " + res)
    //new NetIncome()(res.asInstanceOf[Int])
    new NetIncome()(0)
  }

  class NetIncome extends sim[balance with TaxBurden, NetIncome](0) with number

  val result: dataset[balance with baserate with TaxBurden with NetIncome with TaxTester] = data[balance with baserate with TaxBurden with NetIncome with TaxTester](0)//.calc[TaxBurden]//.calc[TaxBurden].calc[TaxBurden] //.calc[OtherThing].calc[T]


  //  val performanceTest = (0 until 1000).foldLeft(new Col[one with two with T])((a, c) => a.calc[T])
  //  (new Col[two with T]).calc[OtherThing]

  def main(args: Array[String]): Unit = {
    val test = result.calc[TaxTester].calc[TaxBurden].calc[TaxTester].calc[TaxBurden].calc[TaxTester].calc[TaxBurden].calc[TaxBurden]
    println("testval " + test.initialVal)
  }
}
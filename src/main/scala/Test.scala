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
    class eps extends axiom[Double,eps](.000003)

    val X_f = (src:dataset[X with A]) =>{
      val x = src.fetchDouble[X]
      val a = src.fetchDouble[A]
      ((x*x) + a)/(x*2)
      //x + x
    }
  val X_func = X_f.set[X]

  class X extends recSim[Double,X,X with A](X_func)(1d)



  trait Converges[dep<:dataset[_],U<:model[dep,U]]{
    val f:(Double,model[dep,U]) => Int
  }

  //trait Convergent[A<:model[_,A]] extends Converges[A]

  class doesConverge[self<:doesConverge[self,_,_] with model[_,self] with InitialType[Boolean,self] with reset[Boolean,self],dep<:dataset[self],U<:model[dep,U] with InitialType[Double,U]](
    eps:Double,override val f:(Double,model[dep,U]) => Int,ctx:provider[_]
   )(
      implicit override val tag: ClassTag[self],tagu:ClassTag[U]//,ctx:provider[_]
    ) extends recSim[Boolean,self,dep](
    (
        (src:dataset[dep]) => {
            val res = (0 until f(eps,build[U])).foldLeft[dataset[dep with self with U]](
              data[dep with self with U](ctx)
            )( (a,c) => a.calcDouble[U])
          val n0 = res.initialVal.asInstanceOf[Double]
          val n1 = res.calcDouble[U].typedInitVal
            scala.math.abs(n1 - n0) < eps
        }
    ).set[self]
    )(false)(tag,ctx) with Converges[dep,U]



  implicit val ctx = myprovider.register[A].register[X].register[eps]
  class XConverges extends doesConverge[XConverges,X with A with XConverges,X](0.00002,(_,_) => n,ctx)
  val n = 1000

  lazy val performanceTest = (0 until n).foldLeft[dataset[A with X with XConverges]](
    data[A with X with XConverges](ctx)
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
    println(s"Does X converge :" + performanceTest.calc[Boolean,XConverges].initialVal)
//    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
//    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
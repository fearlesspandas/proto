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
      //println("x func")
      val x = src.fetchDouble[X]
      val a = src.fetchDouble[A]
      ((x*x) + a)/(x*2)
      //x + x
      //x + x
    }
  val X_func = X_f.set[X]

  class X extends recSim[Double,X,X with A](X_func)(1d)



  trait Converges[dep<:dataset[_],U<:model[dep,U]]{
    val f:(Double,model[dep,U]) => Int
  }

  //trait Convergent[A<:model[_,A]] extends Converges[A]

  class looksConvergent[self<:looksConvergent[self,_,_] with model[_,self] with InitialType[Boolean,self] with reset[Boolean,self],dep<:dataset[self],target<:model[dep,target] with InitialType[Double,target]](
      eps:Double
                                                                                                                                                                                                                )(
      implicit override val tag: ClassTag[self],tagu:ClassTag[target]//,ctx:provider[_]
    ) extends recSim[Boolean,self,dep with self with target](
    (
        (src:dataset[dep with self with target]) => {
           val wasConvergenc = src.fetchBool[self]
            val lastval = src.fetchDouble[target]
            val nextval = src.calcIter[Double,target](20).fetchDouble[target]//calcIter[Double,target](10).fetchDouble[target]

            val res = wasConvergenc.typedInitVal || scala.math.abs((lastval - nextval).typedInitVal) < eps
            println(s"lastval: ${lastval.initialVal} nextval: ${nextval.initialVal} epsilon:$eps")
          res
        }
    ).set[self]
    )(false)


  class sum[self<:sum[self,_,_] with model[_,self] with InitialType[Double,self] with reset[Double,self],dep<:dataset[target] with InitialType[_,_],target<:model[dep,target] with InitialType[Double,target]]
  (
    implicit tagself:ClassTag[self],tagtarget:ClassTag[target]
                                                                                                                                                                                                              ) extends recSim[Double,self,dep with self with target](
        (
          (src:dataset[dep with self with target]) => {
            val currsum = src.fetchDouble[self]
            val nextval = src.fetchDouble[target]
            currsum + nextval
          }
          ).set[self]
        )(0d)



  implicit val ctx = myprovider.register[A].register[X].register[eps].register[XSum].register[XConverges]
  class XConverges extends looksConvergent[XConverges,X with A with XConverges,X](.2d)
  class XSum extends sum[XSum,X with A with XSum,X]
  val k = 2

  lazy val performanceTest = (0 until k).foldLeft[dataset[A with X with XConverges with XSum]](data[A with X with XConverges with XSum](ctx))( (a,c) => a.calc[Double,X].calc[Double,XSum].calc[Boolean,XConverges])

//  lazy val performanceTest2 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](
//    data[balance with baserate with TaxBurden with mylist](ctx)
//  )( (a, c) => a.calc[Double,TaxBurden])
//
//
//  lazy val performanceTest3 = (0 until n).foldLeft[dataset[balance with baserate with TaxBurden with mylist]](
//    data[balance with baserate with TaxBurden with mylist](performanceTest.dataprovider())
//  )( (a, c) => a.calc[Double,TaxBurden])

  def main(args: Array[String]): Unit = {
    val t = k + 1
    val t0 = System.nanoTime()
    println("Starting performance test")
    println(s"X after $k iterations " + performanceTest.fetchDouble[X].initialVal)
    println(s"Sum of X :" + performanceTest.fetchDouble[XSum].initialVal)
    println(s"Does X converge ? " + performanceTest.fetchBool[XConverges].initialVal)

    //    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
//    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
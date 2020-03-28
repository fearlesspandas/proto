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

  class looksConvergent[self<:looksConvergent[self,_,_,_] with model[_,self] with InitialType[Boolean,self] with reset[Boolean,self],dep<:dataset[_],target<:model[dep,target] with InitialType[Double,target],targetsum<:sum[targetsum,dep,target]](
      eps:Double
                                                                                                                                                                                                                )(
      implicit override val tag: ClassTag[self],tagu:ClassTag[target],tagsum:ClassTag[targetsum]//,ctx:provider[_]
    ) extends recSim[Boolean,self,dep with self with target with targetsum](
    (
        (src:dataset[dep with self with target with targetsum]) => {
           val wasConvergent = src.fetchBool[self]
            val lastval = src.fetchDouble[targetsum]//.typedInitVal
            val nextval = src.calcIter[Double,targetsum](1000).fetchDouble[targetsum]//.typedInitVal
            wasConvergent.typedInitVal || scala.math.abs((lastval - nextval).typedInitVal) < eps
        }
    ).set[self]
    )(false)


  class sum[self<:sum[self,_,_] with model[_,self] with InitialType[Double,self] with reset[Double,self],dep<:dataset[_],target<:model[dep,target] with InitialType[Double,target]]
  (
    implicit val tagtarget:ClassTag[target],tagself:ClassTag[self],dprov:provider[_]
                                                                                                                                                                                                              ) extends recSim[Double,self,dep with self with target](
        (
          (src:dataset[dep with self with target]) => {
            val currsum = src.fetchDouble[self]
            val nextval = src.fetchDouble[target]
            currsum + nextval
          }
          ).set[self]
        )(0d)(tagself,dprov)



  implicit val ctx = myprovider.register[A].register[X].register[eps].register[XConverges].register[XSum]
  class XConverges extends looksConvergent[XConverges,A with X,X,XSum](2.5d)
  class XSum extends sum[XSum,X with A,X]
  val k = 100000

  lazy val performanceTest = (0 until k).foldLeft[dataset[A with X with XConverges with XSum]](data[A with X with XConverges with XSum](ctx))( (a,c) => a.calc[Double,X].calc[Double,XSum])

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
    //println(s"Sum of X :" + performanceTest.fetchDouble[XSum].initialVal)
    println(s"Does X converge ? " + performanceTest.calc[Boolean,XConverges].initialVal)

    //    println(s"Total Tax burden over $n years P2 " + performanceTest2.initialVal)
//    println(s"Total Tax burden over $n years P3 " + performanceTest3.initialVal)
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
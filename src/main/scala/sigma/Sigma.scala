//package sigma
//
//import Typical.core.Typeable._
//import Typical.core.impl._
//import Typical.core.implicits._
//
//import scala.reflect.runtime.{universe => ru}
//import scala.reflect.ClassTag
//
///***
// * Here is an example of how to genericize calculations using Typical.
// * The idea is that we define an operation that is generic across
// * any type in our 'querybase', then in later calculations we can
// * perform our operation generically over any of the types in querybase.
// * This effectively creates a sigma type definition in Typical.
// *
// * In this example we build our Sigma type in a way that allows for the
// * addition of any other desired dependencies in the calculation, providing
// * the programmer peak flexibility in expanding upon this model.
// *
// */
//
//object sigma {
//  class one extends axiom[Double,one](1)
//  class two extends axiom[Double,two](2)
//  class three extends axiom[Double,three](3)
////
////  class SimImpl[init,dependencies,self](
////                                         implicit override val tag: ClassTag[self], dprov: provider[_]
////                                       ) extends sim[init,dependencies,self](_)(_)
//  type querybase = one with two
////
////  class Sigma[
////    T>:querybase<:queryset[Double],
////    self<:modelType[Double,self]]
////  (
////                                                                               implicit tag:ClassTag[T],
////                                                                               ttag:ru.TypeTag[T],
////                                                                               tagself:ClassTag[self],
////                                                                               ttagself:ru.TypeTag[self]
////                                                                             ) extends sim[Double,T with querybase,self](
////    ((src:dataset[T with querybase]) => {
////      src.fetch[Double,T].value + 1 + //Type T is variable and can be any type in querybase
////      src.fetch[Double,one].value     //This type is not variable and will always be used in the calculation regardless of what T is
////    }).set[self]
////  )(0d)
////
////  class ONE extends Sigma[one,ONE]
////  class aftertest extends sim[Double,ONE with querybase,aftertest](
////    ((src:dataset[ONE with querybase])=> {
////      src.calc[Double,ONE].value
////    }).set[aftertest])(0d)
//
////
////  class Thing[A<:model[B,A],B<:model[A,B]] extends sim[Boolean,A with B,Thing[A,B]](
////    ( (_:dataset[A with B]) => true
////      ).set[Thing[A,B]]
////  )(false)
//
//  class context extends rsim[dataset[context ],context,context](
//    (
//      (src:dataset[context]) => {
//        class temp1 extends sim[Boolean,temp2,temp1](null)(false)
//        class temp2 extends sim[Boolean,temp1,temp2](null)(false)
//        val ct = src.fetch[dataset[context], context]
//        ct
//          .include[Boolean,temp1](true)
//          .include[Boolean,temp2](true)
//          //.asInstanceOf[dataset[context]]
//      }
//    ).set[context]
//  )(null)
//
//  def main(args: Array[String]): Unit = {
////    val dat = data[one with two with ONE](baseprovider
////      .register[one]
////      //.register[Sigma[one]]
////    )
////    println(dat.calc[Double,ONE].value)
//  }
//}
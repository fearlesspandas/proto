//package Orders
//
//import Typical.core.Typeable.{dataset, model, number}
//import impl.{axiom, recSim, sim}
//
//import scala.reflect.ClassTag
//
////import Typical.implicits.implicits._
//object Order {
//
//  import Typical.implicits.implicits._
//  implicit object myprovider extends number
//
//  //below is an implementation of a basic order matching system
//  //defined very generally across essentially arbitrary types.
//  //Orders in this system are then placed by calling the standard
//  //calc method on the expected type of the order
//  implicit val tx1 : dataset[two with one] => dataset[one] = (src:dataset[two with one]) => new one()(src.fetch[one].initialVal.asInstanceOf[Double] - 1d)
//  implicit val tx2 : dataset[one with two] => dataset[two] = null
//
//  class one extends recSim[one,two with one](tx1)(1d) with number with
//  class two extends recSim[two,one with two](tx2)(1d) with number
//  class ordermatch[A<:model[B with A,A],B<:model[A with B,B]]( //due to the identity mapping we can always say any A is a dependency of itself
//                                                 override implicit val iterateFrom: dataset[A with B] => dataset[ordermatch[A, B]],
//                                                 override implicit val tag: ClassTag[ordermatch[A, B]]
//                                               ) extends sim[A with B,ordermatch[A,B]](0){
//    def fill() = {
//      //find owners of the A and B calculation functions
//      //call calc on A and B to fill them as specified
//      //give owners 1 of A and B respectively
//    }
//
//  }
//  implicit val matching: dataset[one with two] => dataset[ordermatch[one,two]] = null
//  //ToDo implement the following in 'matching' function
//  //and make it's structur producable by a factory.
//
//  //find owners of the A and B calculation functions
//  //call calc on A and B to fill them as specified
//  //give owners 1 of A and B respectively
//
//  new ordermatch[one,two]() //proves that one and two can fill eachother
//                                                  //calling calc on an ordermatch should then trigger the
//                                                  //the atomic fill of the order
//
//}

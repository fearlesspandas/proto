package Orders

import Typical.core.Typeable._
import impl._
import scala.reflect.ClassTag
import Typical.implicits.implicits._
//import Typical.implicits.implicits._
object Order {
  //below is an implementation of a basic order matching system
  //defined very generally across essentially arbitrary types.
  //Orders in this system are then placed by calling the standard
  //calc method on the expected type of the order
  implicit val tx1 = (src:dataset[two with one]) => src.fetchDouble[one] - 1d
  implicit val tx2 = (src:dataset[one with two]) => src.fetchDouble[two] - 1d

  class one extends recSim[Double,one,two with one](tx1.set[one])(1d)
  class two extends recSim[Double,two,one with two](tx2.set[two])(1d)
  class ordermatch[A<:model[B with A,A],B<:model[A with B,B]](implicit override val iterateFrom: dataset[A with B] => dataset[ordermatch[A, B]]) extends sim[Double,A with B,ordermatch[A,B]](0d){
    def fill() = {
      //find owners of the A and B calculation functions
      //call calc on A and B to fill them as specified
      //give owners 1 of A and B respectively
    }

  }
  implicit val matching: dataset[one with two] => dataset[ordermatch[one,two]] = null
  //ToDo implement the following in 'matching' function
  //and make it's structur producable by a factory.

  //find owners of the A and B calculation functions
  //call calc on A and B to fill them as specified
  //give owners 1 of A and B respectively

  new ordermatch[one,two]() //proves that one and two can fill eachother
                                                  //calling calc on an ordermatch should then trigger the
                                                  //the atomic fill of the order

}

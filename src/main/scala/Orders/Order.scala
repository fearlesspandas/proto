package Orders

import Typical.core.Typeable._
import Typical.impl._

import scala.reflect.ClassTag
import Typical.implicits.implicits._

import scala.collection.immutable.HashMap
//import Typical.implicits.implicits._
object Order {

  //below is an implementation of a basic order matching system
  //defined very generally across essentially arbitrary types.
  //Orders in this system are then placed by calling the standard
  //calc method on the expected type of the order

  //case class order[A,B]()

  trait GloballyOrdered[A<:Ordered[A]] extends Ordered[A] {
    def compareAny(that:Any):Int
    override def compare(that: A): Int = compareAny(that)
  }

  case class order[price <: GloballyOrdered[price], item<:GloballyOrdered[item]](p: price, i: item, remaining: Int,owner:String) {
    def isFilled(other:order[item,price]) = {
      other match {
        case oth@order(_:item,_:price,_,_) => (this.p.compareAny(oth.i) >= 0) && this.i.compareAny(oth.p) >= 0 && this.remaining > 0 //&& this.owner != other.owner
        case _ => false
      }
    }

  }
  def rawType[A<:GloballyOrdered[A],B<:GloballyOrdered[B]](
                                                            src:dataset[
                                                              orderbook[B,A]
                                                                with matching[A,B]
                                                                with matching[B,A]
                                                                with orderbook[A,B]
                                                              //                                                                with escrowadd[A,B]
                                                              //                                                                with escrowadd[B,A]
                                                            ],
                                                            value:order[A,B]
                                                          ):dataset[
    orderbook[B,A]
      with matching[A,B]
      with matching[B,A]
      with orderbook[A,B]
    //      with escrowadd[A,B]
    //      with escrowadd[B,A]
  ]
    with InitialType[_,_]= {
    src
      .include[order[A,B],dataQueue[A,B]](value)
      .calc[Seq[order[B,A]],matching[A,B]]
      .calc[orderbooktype[A,B],orderbook[A,B]]
    //.calc[escrowtype[A,B],escrowadd[A,B]]
  }



  class dataQueue[A<:GloballyOrdered[A],B<:GloballyOrdered[B]] extends axiom[order[A,B],dataQueue[A,B]](null)

  type orderbooktype[A<:GloballyOrdered[A],B<:GloballyOrdered[B]]  = order[A,B] => Map[Any,Seq[order[A,B]]]

  class orderbook[A<:GloballyOrdered[A],B<:GloballyOrdered[B]] extends recSim[
    orderbooktype[A,B],
    orderbook[A,B],
    orderbook[A,B] with dataQueue[A,B] with matching[A,B]
  ](
    ((src:dataset[orderbook[A,B] with dataQueue[A,B] with matching[A,B]]) => {
      val book = src.fetch[orderbooktype[A,B],orderbook[A,B]].typedInitVal
      (o:order[A,B]) => if(o == null) book(null) else {
        val oldbook = book(null)
        oldbook.updated(o.owner,oldbook.getOrElse(o.owner,Seq()) :+ o)
      }
    }
      ).set[orderbook[A,B]]
  )(_ => HashMap())


  //  type escrowtype[A<:GloballyOrdered[A],B<:GloballyOrdered[B]]  = _ => Map[Any,Seq[order[A,B]]]
  //
  //  class escrowadd[A<:GloballyOrdered[A],B<:GloballyOrdered[B]] extends recSim[
  //    escrowtype[A,B],
  //    escrowadd[A,B],
  //    escrowadd[A,B] with dataQueue[A,B] with matching[B,A] with orderbook[A,B]
  //  ](
  //    ((src:dataset[escrowadd[A,B] with dataQueue[A,B] with matching[B,A] with orderbook[A,B]]) => {
  //      val matching = src.fetch[Seq[order[A,B]],matching[B,A]].typedInitVal
  //      val escrow = src.fetch[escrowtype[A,B],escrowadd[A,B]].typedInitVal
  //      val nextOrder = src.fetch[order[A,B],dataQueue[A,B]].typedInitVal
  //      if (matching.size > 0){
  //        val filledOrder_ = matching.head
  //        val filledOrder = filledOrder_.copy(remaining = scala.math.min(nextOrder.remaining,filledOrder_.remaining) )
  //        val oldEscrow = escrow.get(filledOrder.owner).getOrElse(Seq())
  //        val newEscrow = if(oldEscrow.filter(o => o.p == filledOrder.p && o.i == filledOrder.i).size > 0) oldEscrow.collect({
  //          case o:order[A,B] if(o.i == filledOrder.i && o.p == filledOrder.p) => o.copy(remaining = o.remaining + filledOrder.remaining);
  //          case o:order[A,B] => o
  //        }) else oldEscrow :+ filledOrder
  //        escrow.updated(filledOrder.owner,newEscrow)
  //      }else escrow
  //      })
  //      .set[escrowadd[A,B]]
  //  )(HashMap())

  class matching[A<:GloballyOrdered[A],B<:GloballyOrdered[B]] extends recSim[
    order[A,B] => Seq[order[B,A]],
    matching[A,B],
    matching[A,B] with dataQueue[A,B] with orderbook[B,A]
  ]((
    (src:dataset[dataQueue[A,B] with matching[A,B] with orderbook[B,A]]) => {
      val book = src.fetch[orderbooktype[B,A],orderbook[B,A]].typedInitVal
      val nextOrder = src.fetch[order[A,B],dataQueue[A,B]].typedInitVal
      (arg:order[A,B]) => {
        val order = book(order)
        Seq[order[B,A]]()
      }
      //      val matchingorders = book.values
      //        .flatMap(x => x)
      //        .filter(x => x.remaining > 0)
      //        .filter(x => x.isFilled(nextOrder))
      //      matchingorders.toSeq
    }).set[matching[A,B]])(null)

}

import Typical.core.typeable
import Typical.core.typeable.{Id, axiom, dataset, idtype, model, modelBase, produces}
//package Typical
//
//import Typical.core.Typeable._
//import Typical.core.impl._
//
//import Typical.core.implicits._
//
//import scala.collection.immutable.HashMap
//package object Order {
//  import scala.math._
//  //below is an implementation of a basic order matching system
//  //defined very generally across essentially arbitrary types.
//  //Orders in this system are then placed by calling the standard
//  //calc method on the expected type of the order
//
//
//  trait GloballyOrdered[A] extends Ordered[A] {
//    def compareAny(that:Any):Int
//    override def compare(that: A): Int = compareAny(that)
//  }
//
//  case class order(p: GloballyOrdered[_], i: GloballyOrdered[_], remaining: Int,owner:String) {
//    def isFilled(other:order) = (this.p.compareAny(other.i) >= 0) && this.i.compareAny(other.p) >= 0 && this.remaining > 0 && this.owner != other.owner
//
//    def inKind(other:order):Boolean = if(other.p.compareAny(p) == 0  && other.i.compareAny(i) == 0 && other.owner == owner) true else false
//  }
//
//
//  type orderbooktype  = order => Map[Any,Seq[order]]
//
//  class orderbook extends rsim[
//    orderbooktype,
//    orderbook with matching,
//    orderbook
//  ](
//    ((src:dataset[orderbook  with matching]) => {
//      val book = src.fetch[orderbooktype,orderbook].value
//      val func = (o:order) => {
//        val oldbook = book(o)
//        if(o == null) oldbook else {
//          val (remainingorder, matches) = src.calc[order => (order, Seq[order]), matching].value(o)
//          val matchedbook = matches.foldLeft(oldbook)((bk, ord) => {
//            val orderOwnerOrders = bk.getOrElse(ord.owner, Seq())
//            val updatedOrders: Seq[order] = orderOwnerOrders.map(x => if (x.inKind(ord)) order(x.p, x.i, max(0, x.remaining - ord.remaining), x.owner) else x)
//            bk.updated(ord.owner, updatedOrders)
//          })
//          val remainingownerOrders = matchedbook.getOrElse(remainingorder.owner, Seq())
//          val hasInKindOrder = remainingownerOrders.filter(_.inKind(remainingorder)).size > 0
//          val remainingownerupdatedorders = hasInKindOrder match {
//            case true => remainingownerOrders.map(x => if (x.inKind(remainingorder)) order(x.p, x.i, max(0, x.remaining + remainingorder.remaining), x.owner) else x)
//            case _ => remainingownerOrders :+ remainingorder
//          }
//          matchedbook.updated(remainingorder.owner,remainingownerupdatedorders)
//        }
//      }
//      func
//    }).set[orderbook]
//  )(_ => HashMap())
//
//
//
//  type matchtype = order => (order,Seq[order])
//  class matching extends rsim[
//    matchtype,
//    matching with orderbook,
//    matching
//  ]((
//    (src:dataset[matching with orderbook]) => {
//      val bookfunc = src.fetch[orderbooktype,orderbook].value
//      (arg:order) => {
//        if (arg == null) null
//        else {
//          val book = bookfunc(null)
//          val initargs = (arg.remaining, Seq[order]())
//          val (unallocated, matchingorders) = book.values.flatMap(x => x).foldLeft(initargs)((acc, curr) => acc match {
//            case (remaining, orders) if (remaining > 0 && curr.isFilled(arg)) => {
//              val fillamount = min(min(arg.remaining, curr.remaining), remaining)
//              (
//                max(0, remaining - fillamount),
//                orders :+ order(curr.p, curr.i, fillamount, curr.owner)
//              )
//            }
//            case (remaining, orders) if (remaining > 0) => (remaining, orders)
//            case (0, orders) => (0, orders)
//          })
//          (order(arg.p, arg.i, unallocated, arg.owner), matchingorders)
//        }
//      }
//    }).set[matching])(null)
//
//
//  class EscrowOp
//  case class escrowinput(cmd:EscrowOp,ord:order)
//  case class INJECT[outtype](value:outtype) extends EscrowOp
//  case class ADD() extends EscrowOp
//  case class Fill(p:GloballyOrdered[_],amt:Int)
//
//  type escrowtype = escrowinput => Map[Any,Seq[Fill]]
//  class escrow extends rsim[
//    escrowtype,
//    orderbook with matching with escrow,
//    escrow
//  ](
//    ((src:dataset[orderbook with matching with escrow]) => {
//      val lastescrowfunc = src.fetch[escrowinput => Map[Any,Seq[Fill]],escrow].value
//      (escrw:escrowinput) =>
//        escrw.cmd match{
//          case INJECT(v:Map[Any,Seq[Fill]]) => v
//          case _ =>
//            val lastescrow = lastescrowfunc(escrw)
//            if (escrw.ord == null) lastescrow else {
//              val (remainingorder, matchingorders) = src.fetch[matchtype, matching].value(escrw.ord)
//              val updatedescrow = matchingorders.foldLeft(lastescrow)( (esc,ordr ) => {
//                val ownerescrow = esc.getOrElse(ordr.owner,Seq())
//                val newescrow = ownerescrow :+ Fill(escrw.ord.i,ordr.remaining)
//                esc.updated(ordr.owner,newescrow)
//              })
//              val secondupdatedescrow = matchingorders.foldLeft(updatedescrow)( (esc,ordr ) => {
//                val fillingescrow = esc.getOrElse(escrw.ord.owner,Seq())
//                val filledEscrowUpdated = fillingescrow :+ Fill(escrw.ord.p,ordr.remaining)
//                esc.updated(escrw.ord.owner,filledEscrowUpdated)
//              })
//              secondupdatedescrow
//            }
//
//        }
//
//    }).set[escrow]
//  )(_ => HashMap())
//}
//
import Typical.core.grammar._
//
//case class order[A,B](a:A,b:B)(implicit val f:((A,B) => idtype) with ((B,A) => idtype)) extends modelBase[order[B,A],order[A,B]] with produces[dataset[order[B,A] with order[A,B]]]{
//  val op = order[B,A](b,a)
//  val orderbook = Seq[order[_,_]]
//  def fills(o:order[B,A]):Boolean = ???
//  case class orderid(override val dat: order[B, A]) extends Id[order[B,A]]
//  override def iterate(src: typeable.dataset[order[B, A] with OrderBook]): Option[dataset[order[A, B] with order[B , A]]] = {
//    val filler = src.fetchId[order[B,A]](orderid(op))
//    filler match {
//      case Some(o) => src.includeOp[order[B,A]](None)
//      case None => src.include[order[A,B]](this)
//    }
//  }

//order[thing,stuff].calc[order[stuff,thing]]


//case class order[A,B](a:A,b:B)(implicit val f:((A,B) => idtype) with ((B,A) => idtype)) extends modelBase[order[B,A],order[A,B]] with produces[dataset[order[B,A] with order[A,B]]]{
//  val op = order[B,A](b,a)
//  val orderbook = Seq[order[_,_]]
//  def fills(o:order[B,A]):Boolean = ???
//  case class orderid(override val dat: order[B, A]) extends Id[order[B,A]]
//  override def iterate(src: typeable.dataset[order[B, A] with OrderBook]): Option[order[A,B]] = {
//    val filler = src.fetchId[order[B,A]](orderid(op))
//    filler match {
//      case Some(o) if fills(o) => None
//      case None =>Some()
//    }
//  }
//    for{
//    //fetch orders by unique id
//    filler <- src.fetchId[order[B,A]](orderid(op))
//    orderbook <- src.fetch[OrderBook]
//  }yield {
////    val neworders = orderbook.value.collect({case o:order[B,A] if fills(o) => o})
////    src.include[]
//  }
//  override val id:idtype = f(a,b)
//}
//case class OrderBook(val value:Seq[order[_,_]]) extends axiom[OrderBook,Seq[order[_,_]]] {
//  override def withValue(newVal: Seq[order[_, _]]): OrderBook = OrderBook(newVal)
//}
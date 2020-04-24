package Orders

import Typical.core.Typeable._
import Typical.impl._
import Typical.implicits.implicits._

import scala.collection.immutable.HashMap

//import Typical.implicits.implicits._
object Order {

  //below is an implementation of a basic order matching system
  //defined very generally across essentially arbitrary types using Typical.
  //This feature also illustrates a general use of 'busses', Typicals way of handling real
  //time data injection.

  type orderbooktype[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] = Map[Any, Seq[order[A, B]]]
  type escrowtype[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] = Map[Any, Seq[order[A, B]]]

  def rawType[
    A <: GloballyOrdered[A],
    B <: GloballyOrdered[B]
  ](
     src: dataset[
       orderbook[B, A]
         with matching[A, B]
         with matching[B, A]
         with orderbook[A, B]
     ],
     value: order[A, B]
   ): dataset[
    orderbook[B, A]
      with matching[A, B]
      with matching[B, A]
      with orderbook[A, B]
  ]
    with InitialType[_, _] = {
    src
      .include[order[A, B], dataQueue[A, B]](value)
      .calc[Seq[order[B, A]], matching[A, B]]
      .calc[orderbooktype[A, B], orderbook[A, B]]
  }

  trait GloballyOrdered[A <: Ordered[A]] extends Ordered[A] {
    def compareAny(that: Any): Int

    override def compare(that: A): Int = compareAny(that)
  }

  case class order[price <: GloballyOrdered[price], item <: GloballyOrdered[item]](p: price, i: item, remaining: Int, owner: String) {
    def isFilled(other: order[item, price]) = {
      other match {
        case oth@order(_: item, _: price, _, _) => (this.p.compareAny(oth.i) >= 0) && this.i.compareAny(oth.p) >= 0 && this.owner != other.owner
        case _ => false
      }
    }

  }

  class dataQueue[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] extends axiom[order[A, B], dataQueue[A, B]](null)

  class orderbook[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] extends recSim[
    orderbooktype[A, B],
    orderbook[A, B],
    orderbook[A, B] with dataQueue[A, B] with matching[A, B]
  ](
    ((src: dataset[orderbook[A, B] with dataQueue[A, B] with matching[A, B]]) => {
      val matching = src.fetch[Seq[order[B, A]], matching[A, B]].typedInitVal
      val book = src.fetch[orderbooktype[A, B], orderbook[A, B]].typedInitVal
      val nextOrder = src.fetch[order[A, B], dataQueue[A, B]].typedInitVal
      if (matching.size == 0) {
        val oldportfolio = book.get(nextOrder.owner).getOrElse(Seq())
        val nextportfolio =
          if (oldportfolio.filter(o => o.p == nextOrder.p && o.i == nextOrder.i).size > 0)
            oldportfolio.map(o => if (o.p == nextOrder.p && o.i == nextOrder.i) o.copy(remaining = o.remaining + nextOrder.remaining) else o)
          else oldportfolio :+ nextOrder
        book.updated(nextOrder.owner, nextportfolio)
      } else {
        val filledorder = matching.head
        val newportfolio = book.get(filledorder.owner).getOrElse(Seq()).map(o =>
          if (o == filledorder && nextOrder.remaining < o.remaining) o.copy(remaining = o.remaining - nextOrder.remaining)
          else if (o == filledorder && nextOrder.remaining >= o.remaining) o.copy(remaining = 0)
          else o
        )
        (
          if (nextOrder.remaining >= filledorder.remaining)
            book.updated(nextOrder.owner, book.get(nextOrder.owner).getOrElse(Seq()) :+ nextOrder.copy(remaining = nextOrder.remaining - filledorder.remaining))
          else
            book
          )
          .updated(filledorder.owner, newportfolio)
      }

    }
      ).set[orderbook[A, B]]
  )(HashMap())

  class escrowadd[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] extends recSim[
    escrowtype[A, B],
    escrowadd[A, B],
    escrowadd[A, B] with dataQueue[A, B] with matching[A, B] with orderbook[A, B]
  ](
    ((src: dataset[escrowadd[A, B] with dataQueue[A, B] with matching[A, B] with orderbook[A, B]]) => {
      val escrow = src.fetch[escrowtype[A, B], escrowadd[A, B]].typedInitVal
      val lastbook = src.fetchFromState[orderbooktype[A, B], orderbook[A, B]](src.dataprovider().statestore.size - 2).typedInitVal
      val book = src.fetch[orderbooktype[A, B], orderbook[A, B]].typedInitVal
      val oldorders = lastbook.values.flatMap(i => i)
      val neworders = book.values.flatMap(i => i)
      val filled = oldorders.toSet.diff(neworders.toSet)
      filled.foldLeft(escrow)((a, c) => {
        val lastescrow = a.getOrElse(c.owner, Seq())
        a.updated(c.owner, lastescrow.filter(_ != c))
      })

    }
      ).set[escrowadd[A, B]]
  )(HashMap())

  class matching[A <: GloballyOrdered[A], B <: GloballyOrdered[B]] extends recSim[
    Seq[order[B, A]],
    matching[A, B],
    matching[A, B] with dataQueue[A, B] with orderbook[B, A]
  ]((
    (src: dataset[dataQueue[A, B] with matching[A, B] with orderbook[B, A]]) => {
      val book = src.fetch[orderbooktype[B, A], orderbook[B, A]].typedInitVal
      val nextOrder = src.fetch[order[A, B], dataQueue[A, B]].typedInitVal
      val matchingorders = book.values
        .flatMap(x => x)
        .filter(x => x.remaining > 0)
        .filter(x => x.isFilled(nextOrder))
      matchingorders.toSeq
    }).set[matching[A, B]])(Seq())

}

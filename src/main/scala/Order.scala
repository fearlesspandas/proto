import Typical.core.Typeable._
import impl._
import Typical.implicits.implicits._
object Order {


  class player extends axiom[player](0) with number{
    val isplayer = true
  }
  class item extends axiom[item](0) with number

  class price extends axiom[price](0) with number
  implicit val tx: dataset[player with item with price] => dataset[order] = (src: dataset[player with item with price]) => {
    val p = src.fetch[player]
    new order()(0)
  }

  class order extends sim[player with item with price, order](0)


}

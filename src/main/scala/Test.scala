import Typical.core.Typeable._
import Typical.impl._
import Typical.implicits.implicits._
import scala.collection.immutable.HashMap
object Test {

  case class vertice(pos:Vector[Long],isAlive:Boolean)

  class vertex extends axiom[(vertice,Map[Vector[Long],vertice]) => vertice,vertex](
    (vert:vertice,g:Map[Vector[Long],vertice]) => {
    val grid = g
    val v = vert//grid.get(vert.pos).get
    val neighbors = Seq(
      grid.get(Vector(v.pos(0),v.pos(1) +1)),
      grid.get(Vector(v.pos(0),v.pos(1) -1)),
      grid.get(Vector(v.pos(0) +1,v.pos(1) +1)),
      grid.get(Vector(v.pos(0) -1,v.pos(1) +1)),
      grid.get(Vector(v.pos(0) +1,v.pos(1) -1)),
      grid.get(Vector(v.pos(0) -1,v.pos(1) -1)),
      grid.get(Vector(v.pos(0) +1,v.pos(1))),
      grid.get(Vector(v.pos(0) -1,v.pos(1)))
    ).filter(!_.isEmpty).map(n => n.get)
    val twoNeighbors = neighbors.filter(n => n.isAlive).size == 2
    val threeNeighbors = neighbors.filter(n => n.isAlive).size == 3
    val willbealive = v match {
      case v:vertice if (v.isAlive && twoNeighbors && threeNeighbors)  => true
      case v:vertice if (!v.isAlive && threeNeighbors) =>true
      case _ => false
    }
    vertice(v.pos,willbealive)
  }
  )


  class GridMap extends recSim[
    Map[Vector[Long],vertice],
    GridMap,
    GridMap with vertex
  ](
    ((src:dataset[GridMap with vertex]) => {
      val grid = src.fetch[Map[Vector[Long],vertice],GridMap]
      val griddatasets = grid.typedInitVal.values
      val VertFunc = src.fetch[(vertice,Map[Vector[Long],vertice]) => vertice,vertex].typedInitVal
      val iteratedVertices = griddatasets.map(v => VertFunc(v,grid.typedInitVal))
      iteratedVertices.foldLeft(grid.typedInitVal)( (acc,curr) => acc.updated(curr.pos,curr))

    }).set[GridMap]
  )(HashMap[Vector[Long],vertice]((0 to 100).map( i => (Vector(i + 0l,0l) -> vertice(Vector(i + 0l,0l),true))):_*))


  implicit val ctx = myprovider.register[GridMap].register[vertex]

  val dat = data[GridMap with vertex](ctx).calc[Map[Vector[Long],vertice],GridMap].typedInitVal

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime()
    println("Vertices" + dat.values.toString())
    val t1 = System.nanoTime()
    println("Total time elapsed: " + (t1 - t0)/1000000000.0 + "Sec")
  }
}
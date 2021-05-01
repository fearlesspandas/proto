//package test
//import Typical.core.dataset._
//import Typical.core.grammar._
//import scala.util.Random
//object RandomSim{
//  case class RandomSim(prob:Double,proportion:Double,value:Double) extends model[RandomSim,RandomSim] {
//    override def iterate(src: dataset[RandomSim]): dataset[RandomSim] = for{
//      r <- src.fetch[RandomSim]
//    }yield {
//      if (Random.nextDouble() <= prob){
//
//      }
//      else r
//    }
//  }
//
//}

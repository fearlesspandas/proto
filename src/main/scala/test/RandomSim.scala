//package test
//import Typical.core.dataset._
//import Typical.core.grammar._
//import Consumption._
//import scala.util.Random
//object RandomSim{
//  case class RandomSim(prob:Double,proportion:Double,value:BigDecimal) extends ==>[RandomSim,RandomSim] {
//    override def apply(src: dataset[RandomSim]): dataset[RandomSim] = for{
//      r <- src.<--[RandomSim]
//    }yield {
//      if(r.value <= 0 ) r
//      else if (Random.nextDouble() <= prob){
//        RandomSim(prob,proportion,r.value + r.value*proportion)
//      }
//      else {
//        val bd = r.value - r.value*proportion
//        r.copy(value = if (bd < 0) 0 else bd )
//      }
//    }
//  }
//
//  def fRun(r:RandomSim):RandomSim = {
//    if(r.value <= 0 ) r
//    else if (Random.nextDouble() <= r.prob){
//      RandomSim(r.prob,r.proportion,r.value + r.value*r.proportion)
//    }
//    else {
//      val bd = r.value - r.value*r.proportion
//      r.copy(value = if (bd < 0) 0 else bd )
//    }
//  }
//  def run(src:dataset[RandomSim with Counter],eps:Double):dataset[RandomSim] = try for{
//    r <- src.<--[RandomSim]
//  }yield {
//    if (r.value <= eps) src else run(src.+->[Counter].+->[RandomSim],eps)
//  }catch {
//    case e:Exception =>
//      println("An Error occured")
//      src
//  }
//
//  def run2(randomSim: RandomSim,eps:Double):RandomSim =  if (randomSim.value <= eps) randomSim else run2(fRun(randomSim),eps)
//
//  def main(args:Array[String]):Unit = {
//    val startSim = RandomSim(0.70,0.4,10000000)
//    val dat = data[RandomSim with Counter]()
//      .++(startSim)
//      .++(Counter(0))
//    //val res = (0 to 100000).foldLeft[dataset[RandomSim]](dat)((d,_) => d.calc[RandomSim])
//    val res2 = run2(startSim,1000)
//    println(res2)
//  }
//}

//package test
//import Typical.core.dataset._
//import Typical.core.grammar._
//object GeneSim {
//
//  //entities
//    //genes:Seq[Op]
//  trait input[A<:dataset[A]] extends ::[A]
//
//  //Entity
//  type EntityDeps = In with N
//  trait Entity extends ::[Entity]{
//    val neurons:dataset[GeneSimDeps]
//
//  }
//  case class creature() extends Entity {
//    override val neurons: dataset[GeneSimDeps] = ???
//  }
//
//  val dat = data[Entity]() ++[Entity,creature] creature()
//  val actionNeurons:Seq[Neuron[GeneSimDeps]] = ???
//
//  case class neuronState(weight:Double,active:Boolean)
//  trait Neuron[N<:Entity] extends (N ==> N) with produces[Boolean]{
//    val active:Boolean
//    val value = active
//    val weight:Double
//  }
//
//  trait In extends Neuron[GeneSimDeps]{
//    val neuron = actionNeurons((math.random() * actionNeurons.size).floor.toInt)
//  }
//
//
//    ///....etc
//
//  trait N extends Neuron[GeneSimDeps]
//
//  type GeneSimDeps = Entity with InputN1
//  case class InputN1(active:Boolean,weight:Double) extends In {
//    override def apply(src: dataset[GeneSimDeps]): dataset[GeneSimDeps] = {
//      if(active) src ---> this.neuron else src
//    }
//  }
//
//
//
//  case class Action1() extends Entity{
//  }
//  case class Action2() extends Entity
//}

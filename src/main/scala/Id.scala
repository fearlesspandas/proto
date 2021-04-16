package Typical.core
import Typical.core.typeable.{produces, dataset, model}
import Typical.core.grammar._

package object Id{

//  trait IdGenerator[A,self<:IdGenerator[A,self]] extends model[self,self] with TerminalType[A]
//  case class IdGen() extends IdGenerator[Int,IdGen] {
//    override def iterate(src: dataset[IdGen]): IdGen = new IdGen{
//      override val value: Int = src.fetch[IdGen].get.value + 1
//    }
//
//    override val value: Int = 0
//  }
}
package test
import Typical.core.dataset._
import Typical.core.grammar._

object Employer{
  trait Employer extends ::[Employer]{
    val id:Long
  }
  case class Employers(value:Seq[Employer]) extends ::[Employers]
}


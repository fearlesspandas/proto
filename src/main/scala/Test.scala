import Typical.core._
import Typical.core.typeable.dataset
object runner {
  import typeable._
  import grammar._
  import scala.reflect.runtime.universe._

  case class account()
  case class employer()
  case class input(accounts:Seq[account], employers:Seq[employer])
  class Input extends axiom[Input] with TerminalType[input]
  case class Accounts() extends model[Accounts,Accounts] with TerminalType[Seq[account]]{
    val employerAccounts:Seq[account] = Seq()
    override def iterate(src: dataset[Accounts]): Accounts = {
      val res = src.fetch[Accounts].get
      res
    }

    override def withContext(ctx: contexttype): dataset[Accounts] = {
      val newval = this.value
      new Accounts {
        override val value = newval
        override val context = ctx
      }
    }
  }
  case class Employers() extends model[Employers,Employers] with TerminalType[Seq[employer]]{
    override def iterate(src: dataset[Employers]): Employers = src.fetch[Employers].get
  }
  class GetAccounts extends model[Input,Accounts] with TerminalType[Seq[account]]{
    override def iterate(src:dataset[Input]):Accounts = new Accounts {
      override val value = {
        val in = src.fetchAs[Input,input].get
        in.accounts
      }
    }
  }
  class GetEmployers extends model[Input,Employers] with TerminalType[Seq[employer]] {
    override def iterate(src: dataset[Input]): Employers = new Employers {
      override val value: Seq[employer] = src.fetchAs[Input,input].get.employers
    }
  }
  class CheckEmployerAccounts extends model[Accounts with Employers,Accounts] {
    override def iterate(src: dataset[Accounts with Employers]): Accounts = new Accounts{
      override val value = src.fetchAs[Accounts,Seq[account]].get
      override val employerAccounts: Seq[account] = src
        .fetch[Accounts]
        .get
        .calc[Accounts]
        .fetch[Accounts]
        .get.value
    }
  }


  case class andThen[
    A<:model[A,A],
    B<:model[B,B]
  ](implicit
    atag:TypeTag[A],
    btag:TypeTag[B],
    alltag:TypeTag[andThen[A,B]]
   ) extends directive [A with B,andThen[A,B]] with TerminalType [dataset[A with B]]{
    override def iterate(src: dataset[A with B]): andThen[A,B] = new andThen[A,B] {
      override val value = src.calc[A].calc[B]
    }
  }

implicit val AEdirective = new andThen[Accounts,Employers]
  def main(args:Array[String]):Unit = {
    val dat = data[Employers with Input](
      Map[Any,dataset[_]]()
        .register[Input](
          new Input {
            override val value: input = input(Seq(account()),Seq(employer()))
          }
        )
    )
    val res1 = dat
      .calcFor[GetAccounts,Accounts]
      .calcFor[GetEmployers,Employers]
      val res = res1
      .calcFor[CheckEmployerAccounts,Accounts]
      .calcDirective[Accounts andThen Employers]//.calcFor[ThingThing,Accounts andThen Employers]
      println(res.context.valueView())
    //res.
  }
}
import Typical.core._
object runner {
  import typeable._
  import grammar._


  case class account()
  case class employer()
  case class input(accounts:Seq[account], employers:Seq[employer])
  class Input extends axiom[Input] with InitialType[input]
  class Accounts extends model[Accounts,Accounts] with InitialType[Seq[account]]{
    val employerAccounts:Seq[account] = Seq()
    override def iterate(src: dataset[Accounts]): Accounts = src.fetch[Accounts].get

    override def withContext(ctx: contexttype): dataset[Accounts] = new Accounts {
      override val context = ctx
    }
  }
  class Employers extends model[Employers,Employers] with InitialType[Seq[employer]]{
    override def iterate(src: dataset[Employers]): Employers = src.fetch[Employers].get
  }
  class GetAccounts extends model[Input,Accounts] with InitialType[Seq[account]]{
    override def iterate(src:dataset[Input]):Accounts = new Accounts {
      override val value = {
        val in = src.fetchAs[Input,input].get
        in.accounts
      }
    }
  }
  class GetEmployers extends model[Input,Employers] with InitialType[Seq[employer]] {
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

  def main(args:Array[String]):Unit = {
    val dat = data[Input with Employers](
      Map[Any,dataset[_]]()
        .register[Input](
          new Input {
            override val value: input = input(Seq(account()),Seq(employer()))
          }
        )
    )
    val res = dat.calcFor[GetAccounts,Accounts].calcFor[CheckEmployerAccounts,Accounts]
    println(res.context.valueView())
  }
}
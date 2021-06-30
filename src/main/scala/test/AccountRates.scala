package test
import Typical.core.{dataset, grammar}
import grammar._
import dataset._
import test.Account.{Account, BokerageAccount, CheckingAccount}
case class AccountRates() extends axiom[AccountRates] with produces[Account => Double]{
    override val value:Account => Double = _ match {
        case CheckingAccount(id, balance) => -0.0225
        case BokerageAccount(id, balance) => 0.07
    }
}

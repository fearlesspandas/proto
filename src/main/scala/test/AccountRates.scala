package test
import Typical.core.{dataset, grammar}
import grammar._
import dataset._
import test.Account.Account
case class AccountRates() extends axiom[AccountRates] with produces[Account => Double]{
    override val value:Account => Double = _ => 0.0225
}

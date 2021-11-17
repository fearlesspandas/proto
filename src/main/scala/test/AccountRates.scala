package test
import Typical.core.{dataset, grammar}
import grammar._
import dataset._
import Date._
import test.Account.{Account, BokerageAccount, CheckingAccount}
package object AccountRates{
    type accountRateDeps = Date
    case class AccountRates(baseRateUp:Double,baseRateDown:Double,baseProb:Double) extends ::[AccountRates] with produces[Account => Double]{
        override val value:Account => Double = _ match {
            case CheckingAccount(id, balance,_) => 0
            case BokerageAccount(id, balance,_) =>
                val growAccounts = scala.math.random() > baseProb
                val randomFudge = scala.math.sqrt(scala.math.random())
                if(growAccounts)
                    baseRateUp * randomFudge
                else
                    -1 * baseRateDown * randomFudge
        }
    }
}


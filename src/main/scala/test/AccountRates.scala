package test
import Typical.core.{dataset, grammar}
import grammar._
import dataset._
import Date._
import test.Account.{Account, BokerageAccount, CheckingAccount}
object AccountRates{
    type accountRateDeps = Date
    case class AccountRates(baseRateUp:Double,baseRateDown:Double,baseProb:Double) extends ::[AccountRates] with produces[Account => Double]{
        override val value:Account => Double = _ match {
            case CheckingAccount(id, balance) => 0
            case BokerageAccount(id, balance) =>
                val growAccounts = scala.math.random() > baseProb
                val randomFudgeUp = scala.math.sqrt(scala.math.random())
                val randomFudgeDown = scala.math.random()
                if(growAccounts)
                    baseRateUp * randomFudgeUp
                else
                    -1 * baseRateDown * randomFudgeUp
        }
    }
}


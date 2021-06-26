package test

import Typical.core.dataset.dataset
import test.Account.{Account, Accounts}

object Actions {
type Action[A,B<:dataset[_]] = A => dataset[B]

}

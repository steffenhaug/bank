import exceptions._

// Auto-incrementing ID system for bank accounts.
// this is used to construct a complete ordering
// of bank accounts to avoid deadlocks as
// described in the "learn concurrency..." book.
object AccountID {
    var next: Int = 0

    def id(): Int = this.synchronized {
        next += 1
        next
    }
}

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    // Just a type alias to make it obvious which side is error.
    type ErrMsg = String

    val balance = new Balance(initialBalance)
    val uid     = AccountID.id

    // We need to do the entire if-else in one synchronized evaluation, or
    // else we risk another thread modifying the balance so the branch we
    // chose is no longer correct!

    def withdraw(amount: Double): Either[Unit, ErrMsg] = this.synchronized {
        if (amount > balance.amount) {
            Right("Insufficient balance!")
        } else if (amount < 0) {
            Right("Can not withdraw negative amount!")
        } else {
            Left(balance.amount -= amount)
        }
    }

    def deposit (amount: Double): Either[Unit, ErrMsg] = this.synchronized {
        if (amount < 0) {
            Right("Can not deposit negative amount!")
        } else {
            Left(balance.amount += amount)
        }
    }

    def getBalanceAmount: Double = this.synchronized {
        balance.amount
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }
}

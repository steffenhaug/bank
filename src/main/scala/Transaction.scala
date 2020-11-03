import exceptions._
import scala.collection.mutable
import scala.collection.mutable.Queue

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  // TODO
  // project task 1.1
  // Add datastructure to contain the transactions.
  //  I used a mutable queue.
  var ts: Queue[Transaction] = Queue()

  // Remove and return the first element from the queue
  def pop: Transaction = ts.synchronized {
    ts.dequeue
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = ts.synchronized {
    ts.length == 0
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = ts.synchronized {
    ts.enqueue(t)
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = ts.synchronized {
    ts.front
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = ts.synchronized {
    ts.iterator
  }
}

class Transaction(
  val from:                   Account,
  val to:                     Account,
  val amount:                 Double,
  val allowedAttemps:         Int
) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempt = 0

  def pending = {
    (status == TransactionStatus.PENDING)
  }

  override def run: Unit = {
    // This does not need synchronization, because it is
    // ran synchronously from a thread, and it is already
    // certain that no two threads can work on the same
    // transaction.
    def doTransaction() = {
      attempt += 1

      // Attempts to perform the transaction.
      from.withdraw(amount) match {
        case Right(_) => {
          // Could not withdraw money.
          // In this case, nothing is changed, so we just
          // fail if necessary, and return.
          if (attempt >= allowedAttemps) {
            status = TransactionStatus.FAILED
          }
        }
        case Left(_) => {
          // We could get money, so deposit it.
          // This can only fail if amount < 0 in which case
          // we alcready failed the withdrawal. This means
          // that we COULD release the mutex on the `from`-
          // account before waiting here IN THIS SPECIFIC
          // EXAMPLE. If it was the case that depositing
          // could fail, we would actually need to use the
          // mutex to put the money back again!
          to.deposit(amount)
          status = TransactionStatus.SUCCESS
        }
      }
    }

    // Start the transactions, locking in the order imposed
    // by the AccountID ordering, c. f. concurrent pr. book
    // on page 46-47.
    if (from.uid < to.uid) {
      from.synchronized { to.synchronized { doTransaction } }
    } else {
      to.synchronized { from.synchronized { doTransaction } }
    }

    Thread.sleep(10) // you might want this to make more room for
                     // new transactions to be added to the queue
  }
}

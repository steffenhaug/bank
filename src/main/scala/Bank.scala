class Bank(val allowedAttempts: Integer = 3) {

  private val tq: TransactionQueue = new TransactionQueue()
  private val pq: TransactionQueue = new TransactionQueue()

  def addTransactionToQueue(
    from:   Account,
    to:     Account,
    amount: Double
  ): Unit = tq.synchronized {
    // Add a new transaction to the queue.
    tq.push(new Transaction(tq, pq, from, to, amount, allowedAttempts))

    // Start processing transactions concurrently.
    // Each thread always completes exactly one transaction, i. e.
    // it runs until it can complete one, or until it can cancel one
    // that has failed too many times.
    //
    // NOTE: Since every thread completes one transaction, and one
    // thread is spawned per transaction, there is always enough threads
    // to complete all the transactions, and also, there is always enough
    // transactions available for the threads to work on.
    new Thread {
      override def run = 
        processTransactions
    }.start
  }

  private def processTransactions: Unit = {
    // Retrieve a task to process.
    // THIS CALL is synchronized on the queue, so the entire
    // execution of `processTransactions` doesnt need to,
    // this would (potentially) deadlock.
    //
    // This means it is not possible for another thread to
    // start processing the same transaction, but does not
    // prevent starting another transaction while processing
    // this one.
    val t: Transaction = tq.pop

    // Start processing this transaction.
    // This might block for a very long time if the transaction
    // takes a long time to acquire the lock on both the bank
    // accounts it is modifying.
    // This is why it is crucial that we don't synchronize on the
    // queue for more than is exactly necessary.
    t.run

    // After it is done executing, check the execution status,
    // and take appropriate action.
    if (t.pending) {
      // If the transaction failed, but needs to be re-tried,
      // it is still pending. In this case, it should be added
      // to the back of queue, and we try to process another
      // transaction first.
      //
      // Note: Re-attempting the same transaction that just failed
      // immediately would obviously be silly; the benefit of the
      // queue is that executing another transaction in the queue
      // first might rectify whatever stopped this one from executing,
      // and thus less transactions would fail.
      tq.push(t)
      processTransactions
    } else {
      // The transaction is not pending, i. e. it is either completed
      // (successfully) or it failed too many times. In this case, it
      // is moved to the list of processed transactions, and is never
      // attempted again.
      pq.push(t)
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    pq.iterator.toList
  }

}

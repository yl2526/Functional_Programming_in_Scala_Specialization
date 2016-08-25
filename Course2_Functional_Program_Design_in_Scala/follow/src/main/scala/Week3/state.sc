// state
class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
}


val acct = new BankAccount

acct deposit 50
acct withdraw 20
acct withdraw 30
//acct withdraw 2

// identity
val x = new BankAccount
val y = new BankAccount // noy the same
// val y = x // same

// operational equivalence : no test can distinguish


for (i <- 1 until 3; j <-"abc") println(i + " " + j)
(1 until 3) foreach (i => "abc" foreach (j => println(i + " " + j)))
trait Publisher {
  private var subscribers: Set[Subscriber] = Set()
  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber
  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber
  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}


trait Subscriber {
  def handler(pub: Publisher)
}

class BankAccount extends Publisher {
  private var balance = 0
  def currentBalance: Int = balance
  def deposit(amount: Int): Unit =
  if (amount > 0) {
    balance = balance + amount
    publish()
  }
  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
    } else throw new Error("insufficient funds")
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  private var total: Int = sum()
  private def sum() =
    observed.map(_.currentBalance).sum
  def handler(pub: Publisher) = sum()
  def totalBalance = total
}

val a = new BankAccount
val b = new BankAccount

val c = new Consolidator(List(a, b))

c.totalBalance
a.deposit(20)
c.totalBalance
b.deposit(20)
c.totalBalance


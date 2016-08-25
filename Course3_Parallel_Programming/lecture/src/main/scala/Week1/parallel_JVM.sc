class HelloThread extends Thread {
  override def run() {
    println("Hello")
    println("World")
  }
}

def main() {
  val t = new HelloThread
  val s = new HelloThread

  t.start()
  s.start()

  t.join()
  s.join()
}
main()

// un-automic
private var uidCount = 0L
def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }
  t.start()
  t
}

startThread(); startThread()

// automicity sychronized
private val monitor = new AnyRef {}
private var uidCountAtomic = 0L
def getUniqueIdAtomic(): Long = monitor.synchronized {
  uidCountAtomic = uidCountAtomic + 1
  uidCountAtomic
}

def startThreadAtomic() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield getUniqueIdAtomic()
      println(uids)
    }
  }
  t.start()
  t
}

startThreadAtomic(); startThreadAtomic()

class Account(private var amount: Int = 0) {
  def transfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
}
def startAccountThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) {
        a.transfer(b, 1)
      }
      println(uids)
    }
  }
  t.start()
  t
}

// Deadlock
val a = new Account(50)
val b = new Account(70)

val t = startAccountThread(a, b, 15)
val s = startAccountThread(b, a, 15)

t.join()
s.join()


private val monitor = new AnyRef {}
private var uidCountAtomic = 0L
def getUniqueIdAtomic(): Long = monitor.synchronized {
  uidCountAtomic = uidCountAtomic + 1
  uidCountAtomic
}

class Account(private var amount: Int = 0) {
  val uid = getUniqueIdAtomic()
  private def lockAndTransfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  def transfer(target: Account, n: Int) =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}
def startAccountThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) {
        a.transfer(b, 1)
      }
      println(uids)
    }
  }
  t.start()
  t
}

val a = new Account(50)
val b = new Account(70)

val t = startAccountThread(a, b, 15)
val s = startAccountThread(b, a, 15)

t.join()
s.join()
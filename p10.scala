@main def main(): Unit ={
  println("========== Question 1 ==========")

  var x = Rational(5, 6)

  println("x: " + x)
  println("x.neg: " + x.neg)

  println("\n\n========== Question 2 ==========")

  x = Rational(5, 6)
  var y = Rational(1, 2)
  var z = Rational(4, 9)

  println("x: " + x)
  println("y: " + y)
  println("z: " + z)
  println("x - y - z: " + x.sub(y).sub(z))

  println("\n\n========== Question 3 ==========")

  var acc1 = Account()
  acc1.deposit(2000)
  var acc2 = Account()
  acc2.deposit(3000)

  println("Before transfer:")
  println("Account 1 balance: " + acc1.getBalance)
  println("Account 2 balance: " + acc2.getBalance)

  acc1.transfer(250, acc2)

  println("\nAfter transfer: (transfer 250 from Account 1 to Account 2)")
  println("Account 1 balance: " + acc1.getBalance)
  println("Account 2 balance: " + acc2.getBalance)

  println("\n\n========== Question 4 ==========")

  acc1 = Account()
  acc1.deposit(2000)
  acc2 = Account()
  acc2.withdraw(300)
  val bank = Bank()
  bank.addAccount(acc1)
  bank.addAccount(acc2)

  println("Accounts: " + bank)
  println("\nNegative balance accounts: " + bank.negativeBalanceAccounts)
  println("Total balance: " + bank.totalBalance)
  println("Total balance with interest: " + bank.totalBalanceWithInterest)
  println("Accounts after interest: " + bank)

  println("\n\n========== Question 5 ==========")

  val strings = List("cricket", "football", "rugby", "tennis")
  println("Strings: " + strings)
  println("Total count of letter occurrences: " + countLetterOccurrences(strings))
}


class Rational(n: Int, d: Int = 1) {
  require(d != 0, "Denominator must be non-zero")
  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def neg: Rational = new Rational(-numer, denom)

  def sub(other: Rational): Rational = new Rational(
    numer * other.denom - other.numer * denom,
    denom * other.denom
  )

  override def toString: String = s"$numer/$denom"

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}


class Account {
    private var balance: Double = 0
    def deposit(amount: Double): Unit =
        balance += amount
    def withdraw(amount: Double): Unit =
        balance -= amount
    def transfer(amount: Double, to: Account): Unit =
        this.withdraw(amount)
        to.deposit(amount)
    def getBalance: Double =
        balance
        
    override def toString(): String = s"Account balance: $balance"
}


class Bank {
  private var accounts = List[Account]()
  
  def addAccount(acc: Account): Unit = {
    accounts = acc :: accounts
  }

  def negativeBalanceAccounts: List[Account] = {
    accounts.filter(_.getBalance < 0)
  }

  def totalBalance: Double = {
    accounts.map(_.getBalance).sum
  }

  def applyInterest(): Unit = {
    accounts.foreach((acc) => {
      if (acc.getBalance >= 0) {
        acc.deposit(acc.getBalance * 0.05)
      } else {
        acc.deposit(acc.getBalance * 0.10)
      }
    })
  }

  def totalBalanceWithInterest: Double = {
    applyInterest()
    accounts.map(_.getBalance).sum
  }

  override def toString: String = {
    accounts.toString()
  }
}

def countLetterOccurrences(strings: List[String]): Int =
  strings.map(_.length).reduce(_ + _)

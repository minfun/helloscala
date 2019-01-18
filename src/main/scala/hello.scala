import scala.annotation.tailrec

//def fib(n: Int): Int = {
//  @annotation.tailrec
//  def loop(n: Int, prev: Int, cur: Int): Int =
//    if (n <= 1) prev
//    else loop(n -1, cur, prev + cur)
//  loop(n, 0, 1)
//}
object Main {
  def main(args: Array[String]): Unit =
    println("Hello, Scala developer!")
  MyProgram.myMethod
}


object helloscala extends App {
  def hello(args: List[String]): Unit =
    println("Hello, Scala developer!" + args(0))
  hello(List("a", "b", "c"))
  MyProgram.myMethod
}

object Hello extends App {
  def square(x: Double) = x * x
  def sumOfSquares(x: Double, y: Double) = square(x) + square(y)
  sumOfSquares(1, 2)
  println(sumOfSquares(1, 2))
}


object World extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = if (n <= 0) prev else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }
  println(fib(5))
}

case class Point(x: Int, y: Int)
case class Book(isbn: String)
case class Message(sender: String, recipient: String, body: String)

object Ha extends App {
  println(1.to(10))
  println(3 + 2)
  println(3.+(2))
  println(1 to 10)
  println("hah")
  println(16.toHexString)
  println((0 to 10).contains(10))
  println(0 until 10)
  println("foo".drop(1))
  println("bar".take(2))
  println(Point(1, 2))
  val x = Point(1, 2)
  val y = Point(1, 2)
  println(x == y)
  val frankenstein = Book("978-12345")
  println(frankenstein)
  val message1 = Message("leifan@gmail.com", "wang@gamil.com", "good luck")
  println(message1.sender)
  println(message1.recipient)
  println(message1.body)
  val message2 = Message("hello@gmail.com", "world@gamil.com", "hello world")
  println(message2.sender)
  println(message2.recipient)
  println(message2.body)
  val message3 = Message("h@h.com", "w@w.com", "hw")
  val message4 = Message("h@h.com", "w@w.com", "hw")
  val messagesAreTheSame = message3 == message4
  println(messagesAreTheSame)
  val message5 = Message("helloworld@gmail.com", "minfun@gamil.com", "good luck")
  val message6 = message5.copy(message5.sender, recipient = "nowican@gamil.com")
  println("------")
  println(message5)
  println(message6)
}


class Greeter(prefix: String, suffix: String) {
  def greet(name: String): Unit =
    println(prefix + name + suffix)
}


object min extends App {
  def addFunc(x: Int, y: Int): Int = x + y
  def addThenMultiply(x: Int, y: Int)(multiplier: Int): Int = (x + y) * multiplier
  def name: String = System.getProperty("user.name")
  def getSquareString(input: Double): String = {
    val square = input * input
    square.toString
  }
  val greeter = new Greeter("Hello, ", "!")
  greeter.greet("Scala developer")
  println(((x: Int) => x + 1)(2))
  val addOne = (x: Int) => x + 1
  println(addOne(3))
  val add = (x: Int, y: Int) => x + y
  println(add(5, 5))
  val getTheAnswer = () => 52
  println(getTheAnswer())
  println(addFunc(8, 9))
  println(addThenMultiply(2, 5)(10))
  println("Hello, " + name + "!")
  println(getSquareString(3))
}


object fun extends App {
  object IdFactory {
    private var counter = 0
    def create(): Int = {
      counter += 1
      counter
    }
  }

  val newId: Int = IdFactory.create()
  println(newId)
  val newerId: Int = IdFactory.create()
  println(newerId)
  val newererId: Int = IdFactory.create()
  println(newererId)
}


object good extends App {
  trait Greeter {
    def greet(name: String): Unit =
      println("Hello, " + name + "!")
  }

  class DefaultGreeter extends Greeter
  class CustomizableGreeter(prefix: String, postfix: String) extends Greeter {
    override def greet(name: String): Unit = {
      println(prefix + name + postfix)
    }
  }

  val greeter = new DefaultGreeter()
  greeter.greet("Scala developer")
  val customGreeter = new CustomizableGreeter("How are you, ", "?")
  customGreeter.greet("Scala developer")
}


object luck extends App {
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)
  def improve(guess: Double, x: Double) = (guess + x / guess) / 2
  def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) < 0.000001
  def abs(x: Double) = if (x >= 0) x else -x
  def sqrt(x: Double) = sqrtIter(1.0, x)
  println(sqrtIter(2, 5))
  println(sqrtIter(3, 9))
  println(sqrtIter(2, 25))
  println(sqrtIter(5, 25))
  println(sqrt(5))
  println(sqrt(9))
  println(sqrt(25))
}


object night extends App {
  def factorial(n: Int): Int =
    if (n == 1) n
    else factorial(n -1) * n
  println(factorial(3))
  println(factorial(6))
}


object fibn extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n <= 0) prev else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }
  println(fib(0))
  println(fib(1))
  println("011")
  println(fib(2))
  println("201")
  println("111")
  println("012")
  println(fib(3))
  println("301")
  println("211")
  println("112")
  println("023")
  println(fib(4))
  println(fib(5))
  println(fib(6))
  println(fib(7))
  println(fib(8))
}


object isSorted extends App {
  def mySorted[A](a: Array[A], ordering: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= a.length - 1) true
      else if (ordering(a(n), a(n + 1))) false
      else go(n + 1)
    go(0)
  }
  println(mySorted(Array(1, 3, 5, 7), (x: Int, y: Int) => x > y))
  println(mySorted(Array(3, 1, 5, 7), (x: Int, y: Int) => x > y))
  println(mySorted(Array("haha", "he", "h"), (x: String, y: String) => x.length < y.length))
}


object Currying extends App {
  def curry[A, B, C](x: (A, B) => C): A => B => C =
    a => b => x(a, b)
  def uncurry[A, B, C](x: A => B => C): (A, B) => C =
    (a, b) => x(a)(b)
  def compose[A, B, C](x: B => C, y: A => B): A => C =
    a => x(y(a))
  def f(a: Int, b: Int): Int = a + b
  def g(a: Int)(b: Int): Int = a + b
  def h(a: String, b: String): String = a + b
  def m(a: Int): Int = a / 2
  def n(a: Int): Int = a + 2
  println(curry(f)(1)(1))
  println(curry(f)(1)(1))
  println(f(1, 1))
  println(g(1)(1))
  println(curry(f)(1)(2))
  println(curry(h)("Hello, ")("World!"))
  println(uncurry(g)(1, 1))
  println(compose(m, n)(0))
  println(compose(m, n)(2))
  println(compose(n, m)(2))
}


object anyList extends App {
  val list: List[Any] = List(
    "a string",
    123,
    'c',
    () => "anoymous function of string"
  )
  list.foreach(element => println(element))
}


object castType extends App {
  val x: Long = 987654321
  val y: Float = x // some precision is lost
  val face: Char = '☺'
  val number: Int = face
  //  val z: Long = y can not covert Float to Long
  println(x)
  println(y)
  println(face)
  println(number)
}


object fact extends App {
  def factor(n: Int): Int = if (n>=1) n else factor(n - 1) * n
  println(factor(3))
  println(factor(5))
}


object sqrt extends App {
  def sqrtIter(guess: Double, x: Double): Double = if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)
  def isGoodEnough(guess: Double, x: Double): Boolean = if (abs(guess * guess - x) < 0.0001) true else false
  def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2
  def abs(x: Double): Double = if (x >= 0) x else -x
  println(sqrtIter(3, 9))
  println(sqrtIter(2, 4))
  println(sqrtIter(2, 5))
}


object sqrt2 extends App {
  def sqrt(x: Double): Double = {
    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) < 0.00001
    def abs(x: Double): Double =
      if (x > 0) x else -x
    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)
    sqrtIter(1.0, x)
  }
  println(sqrt(2))
  println(sqrt(5))
  println(sqrt(9))
}


object scope extends App {
  val x = 0
  def f(y: Int) = y + 1
  val result = {
    val x = f(3)
    x * x
  } + x
  println(result)
  val y = x + 1; y * y
  println(y)
}


object MyProgram {
  val myVal = 123
  def myMethod = println("my program")
}


object gcd extends App {
  @tailrec
  def gcd(x: Int, y: Int): Int =
    if (y == 0) x else gcd(y, x % y)
  println(gcd(14, 21))
  println(gcd(11, 121))
  println(gcd(3, 10))
}


object fac extends App {
  def factorial(x: Int): Int =
    if (x == 1) x else factorial(x - 1) * x
  println(factorial(5))
  println(factorial(6))
}


object fac2 extends App {
  @tailrec
  def factorial(x: Int, y: Int): Int =
    if (x == 1) y else factorial(x - 1, x * y)
  def fact(x: Int): Int = factorial(x, 1)
  println(factorial(5, 1))
  println(factorial(6, 1))
  println(fact(5))
  println(fact(6))
}


object gcd2 extends App {
  @tailrec
  def gcd(x: Int, y: Int): Int =
    if (y == 0) x else gcd(y, x % y)
  println(gcd(5, 7))
  println(gcd(9, 27))
  println(gcd(11, 121))
}
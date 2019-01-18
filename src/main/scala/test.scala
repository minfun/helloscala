
import scala.annotation.tailrec

object haha {
  def main(args: Array[String]) = println("Hello world!")
}

object test extends App {

  case class Note(
                   name: String,
                   duration: String,
                   octave: Int
                 )

  val c3 = Note("C", "Quarter", 3)
  println(c3.name)
  println(c3.duration)
  println(c3.octave)

  sealed trait Symbol
  case class Note2(name: String, duration: String, octave: Int) extends Symbol
  case class Rest(duration: String) extends Symbol
  val symbol1: Symbol = Note2("C", "Quarter", 3)
  val symbol2: Symbol = Rest("Whole")
  println(symbol1)
  println(symbol2)

  def symbolDuration(symbol: Symbol): String =
    symbol match {
      case Note2(name, duration, octave) => duration
      case Rest(duration) => duration
    }
  println(symbolDuration(symbol1))
  println(symbolDuration(symbol2))
}

object traitTest extends App {
  def unexhaustive(): Unit = {
    sealed trait Symbol
    case class Note(name: String, duration: String, octave: Int) extends Symbol
    case class Rest(duration: String) extends Symbol

    def nonExhaustiveDuration(symbol: Symbol): String = symbol match {
      case Rest(duration) => duration
    }
  }
  unexhaustive()
}


object traitEquals extends App {
  case class Note(name: String, duration: String, octave: Int)
  val c3 = Note("C", "Quarter", 3)
  val otherC3 = Note("C", "Quarter", 3)
  val f3 = Note("F", "Quarter", 3)
  println(c3 == otherC3)
  println(c3 == f3)
}

object traitEnumerations extends App {
  sealed trait Symbol
  case class Note(name: String, duration: String, octave: Int) extends Symbol
  val invalidNote = Note("not a name", "not a duration", 3)
  println(invalidNote)
}

object algebraic extends App {
  sealed trait Duration
  case object Whole extends Duration
  case object Half extends Duration
  case object Quarter extends Duration

  def fractionOfWhole(duration: Duration): Double =
    duration match {
      case Whole => 1.0
      case Half => 0.5
      case Quarter => 0.25
    }
  println(fractionOfWhole(Half))
  println(fractionOfWhole(Quarter))
}


object sumall extends App {
  def int(x: Int, y: Int): Int = x
  def sum(x: Int, y: Int): Int =
    if (x > y) 0 else x + sum(x + 1, y)
  println(sum(1, 5))
  println(sum(2, 11))
  @annotation.tailrec
  def tailSum(x: Int, y: Int, v: Int): Int =
    if (x > y) v else tailSum(x + 1, y, x + v)
  println(tailSum(1, 5, 0))
  println(tailSum(2, 11, 0))
  // tailSum(1, 5, 0)
  // tailSum(2, 5, 1)
  // tailSum(3, 5, 1+2)
  // tailSum(4, 5, 1+2+3)
  // tailSum(5, 5, 1+2+3+4)
  // tailSum(6, 5, 1+2+3+4+5)
  def multiple(x: Int, y: Int): Int =
    if (x > y) 1 else x * multiple(x + 1, y)
  println(multiple(1,3))
  println(multiple(2,5))
  @annotation.tailrec
  def tailMultiple(x: Int, y: Int, v: Int): Int =
    if (x > y) v else tailMultiple(x + 1, y, x * v)
  println(tailMultiple(1, 3, 1))
  println(tailMultiple(2, 5, 1))
  def cube(x: Int): Int = x * x * x
  println(cube(5))
  def tailCube(x: Int, y: Int): Int = x * x * x * y
  def sumCube(x: Int, y: Int): Int =
    if (x > y) 0 else x * x * x + sumCube(x + 1, y)
  println(sumCube(1, 3))
  println(sumCube(2, 4))
  @annotation.tailrec
  def sumTailCube(x: Int, y: Int, v: Int): Int =
    if (x > y) v else sumTailCube(x + 1, y, cube(x) + v)
  def factorial(x: Int): Int =
    if (x == 1) 1 else x * factorial(x - 1)
  println(factorial(5))
  @annotation.tailrec
  def tailFactorial(x: Int, v: Int): Int =
    if (x == 1) v else tailFactorial(x - 1, x * v)
  println(tailFactorial(5, 1))
  println(tailFactorial(5, 1) + tailFactorial(4, 1) + tailFactorial(3, 1) + tailFactorial(2, 1) + tailFactorial(1, 1))
  def sumFactorial(x: Int, y: Int): Int =
    if (x > y) 0 else factorial(x) + sumFactorial(x + 1, y)
  println(sumFactorial(1, 5))
  @annotation.tailrec
  def sumTailFactorial(x: Int, y: Int, v: Int): Int =
    if (x > y) v else sumTailFactorial(x + 1, y, tailFactorial(x, 1) + v)
  println(sumTailFactorial(1, 5, 0))
  // sumTailFactorial(1, 5, 1)
  // sumTailFactorial(2, 5, tailFactorial(2, 1) + 1)
  // sumTailFactorial(3, 5, tailFactorial(2, 1) + 1)
  println(sumTailFactorial(1, 1, 0))
  // sumTailFactorial(1, 1, 1)
  // sumTailFactorial(2, 1, tailFactorial(1, 1) + 1)
  // sumTailFactorial(2, 1, 1 + 1)
  println(sumTailFactorial(1, 2, 0))
  println(sumTailFactorial(1, 3, 0))
  //  def sumTailPattern(x: Int, y: Int, v: Int, f: Int => Int):Int =
  //    if (x > y) v else sumTailPattern(x + 1, y, f(x) + v)
  def sumPattern(f: Int => Int, x: Int, y: Int): Int =
    if (x > y) 0 else f(x) + sumPattern(f, x + 1, y)
  def sumPatternInt(x: Int, y: Int): Int = sumPattern(Int => Int, x, y)
  println(sumPatternInt(1, 3))
  println(sumPatternInt(3, 5))
  def sumPatternCube(x: Int, y: Int): Int = sumPattern(Int => Int * Int * Int, x, y)
  println(sumPatternCube(1, 3))
  println(sumPatternCube(2, 3))
  def sumPatternFactorial(x: Int, y: Int): Int = sumPattern(factorial, x, y)
  println(sumPatternFactorial(1, 3))
  println(sumPatternFactorial(2, 5))
  @annotation.tailrec
  def sumTailPattern(f: (Int, Int) => Int, x: Int, y: Int, v: Int): Int =
    if (x > y) v else sumTailPattern(f, x + 1, y, f(x, 1) + v)
  //  @annotation.tailrectailFactorial
  //  def sumTailInt(x: Int, y: Int, v: Int): Int = sumTailPattern(Int, x: Int, y: Int, v: Int)
  def sumTailPatternInt(x: Int, y: Int, v: Int): Int = sumTailPattern(int, x, y, v)
  println(sumTailPatternInt(1, 3, 0))
  println(sumTailPatternInt(3, 5, 0))
  def sumTailPatternCube(x: Int, y: Int, v: Int): Int = sumTailPattern(tailCube, x, y, v)
  println(sumTailPatternCube(1, 3, 0))
  println(sumTailPatternCube(2, 3, 0))
  def sumTailPatternFactorial(x: Int, y: Int, v: Int): Int = sumTailPattern(tailFactorial, x, y, v)
  println(sumTailPatternFactorial(1, 3, 0))
  // sumTailPattern(tailFactorial, 2, 3, tailFactorial(2, 3) + 0)
  // sumTailPattern(tailFactorial, 3, 3, tailFactorial(3, 3) + tailFactorial(2, 3))
  // sumTailPattern(tailFactorial, 4, 3, tailFactorial(3, 3) + 0)
  println(sumTailPatternFactorial(2, 5, 0))
  def sumTailV2(f: Int => Int, a: Int, b: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      if (x > b) acc else loop(x + 1, acc + f(x))
    }
    loop(a, 0)
  }
  println(sumTailV2(Int => Int, 1, 5))
  println(sumTailV2(Int => Int, 1, 10))
}

object list extends App {
  val nums = 1 :: 2 :: 3 :: 4 :: Nil
  val fruit = Nil.::("orange").::("apple").::("banana")
  val fruit2 = "apples" :: ("oranges" :: ("pears" :: Nil))
  println(nums)
  println(fruit)
}

//object sort extends App {
//  def insertionSort(xs: List[Int]): List[Int] = xs match {
//    case List() => List()
//    case y :: ys => insert(y, insertionSort(ys))
//  }
//
//  val cond: (Int, Int) => Boolean =
//    (x: Int, y: Int) => x < y
//
//  def insert(x: Int, xs: List[Int]): List[Int] =
//    xs match {
//      case List() => x :: Nil
//      case y :: ys =>
//        if (cond(x, y)) x :: y :: ys
//        else y :: insert(x, ys)
//    }
//  println(insert(2, 1 :: 3 :: Nil))
//  println(insert(2, 3 :: 1 :: Nil))
//  println(insert(2, 1 :: 3 :: 8 :: 5 :: Nil))
//  println(insertionSort(1 :: 3 :: 8 :: 5 :: Nil))
//  println(insertionSort(1 :: 3 :: 8 :: 5 :: Nil).head)
//  println(insertionSort(1 :: 3 :: 8 :: 5 :: Nil).tail)
//
//  def isort(xs: List[Int]): List[Int] =
//    if (xs.isEmpty) Nil
//    else insert(xs.head, isort(xs.tail))
//
//  def insert(x: Int, xs: List[Int]): List[Int] =
//    if (xs.isEmpty || x <= xs.head) x :: xs
//    else xs.head :: insert(x, xs.tail)
//}
object Main extends App {
  println("Hello, God!")
}

object personal extends App {

  case class Person(name: String, age: Int)

  val l = List(Person("leifan", 25), Person("huan", 24))
  println( l.head.name)
  println( l.head.age)
  println( l(1).name)
  println( l(1).age)
  println( l.lift(3))
}

object personal2 extends App {

}


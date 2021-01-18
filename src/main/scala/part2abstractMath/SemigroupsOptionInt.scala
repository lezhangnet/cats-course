package part2abstractMath

import cats.implicits._

object SemigroupsOptionInt {

  val one: Option[Int] = Option(1)
  val two: Option[Int] = Option(2)
  println("|+|:" + (one |+| two)) // Option(3)

  def main(args: Array[String]): Unit = {
    println("MySemigroupsOptionInt")

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None
    println(one |+| two) // Option(3)
    println(n |+| two) // Option(2)
    println(n |+| n) // None
    println(two |+| n) // Option(2)

  }
}

package part2abstractMath

import cats._
import cats.implicits._

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  val one: Option[Int] = Option(1)
  val two: Option[Int] = Option(2)
  val n: Option[Int] = None
  println("|+|:" + (one |+| two)) // Option(3) // Error:(91, 17) value |+| is not a member of Option[Int] ???

  // TODO 2: implement reduceThings2 with the |+|
  def reduceThings2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println("MySemigroups")

    println(Semigroup[Int].combine(1, 2)) // 3
    println(intCombination) // 48
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String]

    println("--- Option[] ---")
    import cats.instances.option._
    println(Semigroup[Option[Int]].combine(Option(1), None)) // Some(1) ?! NOT None ?!
    // compiler will produce an implicit Semigroup[Option[Int]] whose combine method returns another option with the summed elements
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers
    // compiler will produce an implicit Semigroup[Option[String]] whose combine method returns another option with the concatenated elements
    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))
    // same for any type with an implicit Semigroup

    // test ex 1
    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 10))
    println(reduceThings(expenses))

    // test ex 2
    println(reduceThings2(expenses))

    // import cats.implicits._ // this seems to be eclipse some imports, and will cause |+| to fail below ???
    println(Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6)) // 67

    val aMap       = Map("foo" -> Map("bar" -> 5))
    val anotherMap = Map("foo" -> Map("bar" -> 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
    println(combinedMap) // Map(foo -> Map(bar -> 11))

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None
    println(one |+| two) // Option(3) // Error:(91, 17) value |+| is not a member of Option[Int] ???
    println(n |+| two) // Option(2)
    println(n |+| n) // None
    println(two |+| n) // Option(2)
  }
}

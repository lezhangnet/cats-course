package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  // from a user on slack
  import cats.syntax.either._
  def testNumber2(n: Int): Either[List[String], Int] = {
    val isPrimeError = if (testPrime(n)) None else Some("n must be a prime")
    val isPositiveError = if (n >= 0) None else Some("n must be non-negative")
    val isLessThan100Error = if (n <= 100) None else Some("n must <= 100")
    val isEvenError = if(n % 2 == 0) None else Some("n must be even")
    List(isPrimeError, isPositiveError, isLessThan100Error, isEvenError).flatten match {
      case e @ _ :: _ ⇒ Either.left(e) // i.e. non empty list
      case _ ⇒ Either.right(n)
    }
  }

  // improve from 2 within match{}
  import cats.syntax.either._
  def testNumber3(n: Int): Either[List[String], Int] = {
    val isPrimeError = if (testPrime(n)) None else Some("n must be a prime")
    val isPositiveError = if (n >= 0) None else Some("n must be non-negative")
    val isLessThan100Error = if (n <= 100) None else Some("n must <= 100")
    val isEvenError = if(n % 2 == 0) None else Some("n must be even")
    List(isPrimeError, isPositiveError, isLessThan100Error, isEvenError).flatten match {
      case Nil => Either.right(n)
      case e => Either.left(e) // i.e. non empty list
    }
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
    .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
    .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
    .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither
  // NO toTry

  // TODO 2 - form validation
  object FormValidation {
    import cats.instances.string._
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Passwords must be at least 10 characters long."))

    /*
      fields are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name").andThen(nonBlank(_, "Name"))
      .combine(getValue(form, "Email").andThen(emailProperForm))
      .combine(getValue(form, "Password").andThen(passwordCheck))
      .map(_ => "User registration complete.")
  }

  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError:Validated[String, Int] = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println("MyDataValidation")

    println(testNumber(5))
    println(testNumber2(5))
    println(testNumber2(2))
    println(testNumber3(5))
    println(testNumber3(2))

    println("----- Validate -----")
    println(validateNumber(-5)) // Invalid(List(Number must be even, Number must be non-negative))
    println(validateNumber(2)) // Valid(2)

    val form = Map(
      "Name" -> "",
      "Email" -> "danielrockthejvm.com",
      "Password" -> "Rockthejvm1!"
    )
    println(FormValidation.validateForm(form))

    val goodform = Map(
      "Name" -> "zhale",
      "Email" -> "daniel@rockthejvm.com",
      "Password" -> "Rockthejvm1!"
    )

    println(FormValidation.validateForm(goodform))
  }
}

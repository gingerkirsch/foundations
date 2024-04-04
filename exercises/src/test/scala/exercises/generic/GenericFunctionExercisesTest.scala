package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.temporal.TemporalQueries.localDate
import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0,1).swap == Pair(1,0))
    assert(Pair("John", "Doe").swap == Pair("Doe", "John"))
  }

  test("Pair map") {
    assert(Pair(0,1).map(identity) == Pair(0,1))
  }

  test("Pair zipWith") {
    assert(Pair(0, 1).zipWith(Pair(2, 3))(_ + _) == Pair(2, 4))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  test("Predicate && PBT") {
    forAll{ (eval1: Int => Boolean, eval2: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)

      assert((p1 && False)(value) == false)
      assert((p1 && True)(value) == p1(value))
    }
  }

  test("Predicate || PBT") {
    forAll{ (eval1: Int => Boolean, eval2: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      def False[A]: Predicate[A] = Predicate(_ => false)
      def True[A]: Predicate[A] = Predicate(_ => true)

      assert((p1 || False)(value) == p1(value))
      assert((p1 || True)(value) == true)
    }
  }

  test("Predicate flip") {
    assert(!Predicate.True.flip(()))
    assert(Predicate.False.flip(()))
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false)
    assert(isValidUser(User("john", 20)) == false)
    assert(isValidUser(User("x", 23)) == false)
  }


  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(Try(userIdDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder UserId roundtrip") {
    forAll{(number: Int) =>
      val json = number.toString
      assert(userIdDecoder.decode(json) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2024-04-01\"") == LocalDate.of(2024, 4, 1))
    assert(Try(localDateDecoder.decode("2024-04-01")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder LocalDate roundtrip") {
    forAll(genLocalDate)  { (localDate: LocalDate) =>
      val json = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      localDateDecoder.decode(json) == localDate
    }
  }

  val genLocalDate: Gen[LocalDate] =
    Gen.choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay).map(LocalDate.ofEpochDay)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] =
    Arbitrary(genLocalDate)

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll(genLocalDate)  { (localDate: LocalDate) =>
      val json = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      weirdLocalDateDecoder.decode(json) == localDate
    }
  }

}

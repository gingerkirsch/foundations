package examples

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

object Introduction {

  def reverseStringImperative(x: String): String = {
    val charArray   = x.toCharArray
    val lastIndex   = x.length - 1
    val middleIndex = x.length / 2

    for (i <- 0 until middleIndex) {
      val tmp = charArray(i)
      charArray(i) = charArray(lastIndex - i)
      charArray(lastIndex - i) = tmp
    }

    String.valueOf(charArray)
  }

  def reverseStringFunctional(x: String): String =
    x.foldLeft(List.empty[Char])((acc, c) => c :: acc).mkString

  def reverseStringFunctional2(x: String): String =
    x.foldLeft(new StringBuffer(x.length))(_.insert(0, _)).toString

  def isValidUsername(x: String): Boolean =
    x.length >= 3 && x.forall(_.isLetter)

  def validateUsernamesImperative(xs: List[String]): String = {
    var countError = 0

    for (username <- xs) {
      if (!isValidUsername(username)) countError += 1
    }

    if (xs.isEmpty) "no username"
    else if (countError > 0) s"Found $countError invalid username"
    else "all username are valid"
  }

  def validateUsernamesFunctional(xs: List[String]): String =
    xs.toNel.map(
      _.traverse_(
        username =>
          if (isValidUsername(username)) ().valid
          else 1.invalid
      )
    ) match {
      case None                      => "no username"
      case Some(Invalid(countError)) => s"Found $countError invalid username"
      case Some(Valid(_))            => "all username are valid"
    }

}
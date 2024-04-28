package exercises.errorhandling.option

import exercises.errorhandling.option.OptionExercises.Role._
import exercises.errorhandling.option.OptionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OptionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("getAccountId example") {
    val accountId = AccountId(124)

    assert(Reader(accountId, premiumUser = true).getAccountId.contains(accountId))
    assert(Editor(accountId, "Comic Sans").getAccountId.contains(accountId))
    assert(Admin.getAccountId.isEmpty)
  }

  test("getUserEmail example") {
    val users = Map(
      UserId(222) -> User(UserId(222), "john", Admin, Some(Email("j@x.com"))),
      UserId(123) -> User(UserId(123), "elisa", Admin, Some(Email("e@y.com"))),
      UserId(444) -> User(UserId(444), "bob", Admin, None)
    )

    assert(getUserEmail(UserId(123), users).contains(Email("e@y.com")))
    assert(getUserEmail(UserId(111), users).isEmpty) // no user
    assert(getUserEmail(UserId(444), users).isEmpty) // no email
  }

  test("getAccountIds example") {
    val users = List(
      User(UserId(111), "Eda", Editor(AccountId(555), "Comic Sans"), Some(Email("e@y.com"))),
      User(UserId(222), "Bob", Reader(AccountId(555), premiumUser = true), None),
      User(id = UserId(333), name = "Lea", role = Reader(AccountId(741), premiumUser = false), email = None),
      User(UserId(444), "Jo", Admin, Some(Email("admin@fp-tower.com")))
    )
    assert(getAccountIds(users) == List(AccountId(555), AccountId(741)))
  }

  test("checkAllEmails example success") {
    assert(
      checkAllEmails(
        List(
          User(UserId(111), "Eda", Editor(AccountId(555), "Comic Sans"), Some(Email("e@y.com"))),
          User(UserId(444), "Jo", Admin, Some(Email("admin@fp-tower.com")))
        )
      ).contains(List(Email("e@y.com"), Email("admin@fp-tower.com")))
    )
  }

  test("checkAllEmails example failure") {
    assert(
      checkAllEmails(
        List(
          User(UserId(111), "Eda", Editor(AccountId(555), "Comic Sans"), Some(Email("e@y.com"))),
          User(UserId(222), "Bob", Reader(AccountId(555), premiumUser = true), None),
          User(UserId(333), "Lea", Reader(AccountId(741), premiumUser = false), None),
          User(UserId(444), "Jo", Admin, Some(Email("admin@fp-tower.com")))
        )
      ).isEmpty
    )
  }

  test("sequence example") {
    assert(sequence(List(Some(1), Some(2), Some(3))).contains(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))).isEmpty)
  }

  test("asEditor example") {
    val accountId = AccountId(124)

    assert(Reader(accountId, premiumUser = true).asEditor.isEmpty)
    assert(Editor(accountId, "Comic Sans").asEditor.contains(Editor(accountId, "Comic Sans")))
    assert(Admin.asEditor.isEmpty)
  }
}

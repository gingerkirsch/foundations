package answers.action

import java.time.{Instant, LocalDate}

import org.scalacheck.{Arbitrary, Gen}

object DateGenerator {
  val dateGen: Gen[LocalDate] =
    Gen
      .choose(LocalDate.of(1950,1,1).toEpochDay, LocalDate.MAX.toEpochDay) // edge case of negative year
      .map(LocalDate.ofEpochDay)

  implicit val localDateArb: Arbitrary[LocalDate] =
    Arbitrary(dateGen)

  val instantGen: Gen[Instant] =
    for {
      seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
      nano    <- Gen.choose(0, 1000_000_000L)
    } yield Instant.ofEpochSecond(seconds, nano)

  implicit val instantArb: Arbitrary[Instant] =
    Arbitrary(instantGen)
}

package exercises.dataprocessing

trait Monoid[A] {
  def default: A

  def combine(first: A, second: A): A
}

object Monoid {
  val sumInt: Monoid[Int] = new Monoid[Int] {
    override def default: Int = 0
    override def combine(first: Int, second: Int): Int = first + second
  }

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    override def default: Double = 0.0
    override def combine(first: Double, second: Double): Double = first + second
  }

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override def default: (A, B) = (monoidA.default, monoidB.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) = {
      (
        monoidA.combine(first._1, second._1),
        monoidB.combine(first._2, second._2)
      )
    }
  }

  val sumDoubleInt: Monoid[(Double, Int)] = zip(sumDouble, sumInt)/*new Monoid[(Double, Int)] {
    override def default: (Double, Int) = (0.0, 0)

    override def combine(first: (Double, Int), second: (Double, Int)): (Double, Int) = {
      (
        first._1 + second._1,
        first._2 + second._2
      )
    }
  }*/

  def compareSample(compare: (Sample, Sample) => Sample): Monoid[Option[Sample]]= new Monoid[Option[Sample]] {
    override def default: Option[Sample] = None

    override def combine(first: Option[Sample], second: Option[Sample]): Option[Sample] =
      (first, second) match {
        case (None, None) => None
        case (Some(sample), None) => Some(sample)
        case (None, Some(sample)) => Some(sample)
        case (Some(sample1), Some(sample2)) => Some(compare(sample1, sample2))
      }
  }

  val minSample: Monoid[Option[Sample]] =
    compareSample(
      (sample1, sample2) =>
        if (sample1.temperatureFahrenheit < sample2.temperatureFahrenheit)
          sample1
        else
          sample2
    )
    /*new Monoid[Option[Sample]] {
    override def default: Option[Sample] = None

    override def combine(first: Option[Sample], second: Option[Sample]): Option[Sample] =
      (first, second) match {
        case (None, None) => None
        case (Some(sample), None) => Some(sample)
        case (None, Some(sample)) => Some(sample)
        case (Some(sample1), Some(sample2)) =>
          if (sample1.temperatureFahrenheit < sample2.temperatureFahrenheit)
            Some(sample1)
          else
            Some(sample2)
      }
  }
  }*/

  val maxSample: Monoid[Option[Sample]] =
  compareSample(
    (sample1, sample2) =>
      if (sample1.temperatureFahrenheit > sample2.temperatureFahrenheit)
        sample1
      else
        sample2
  )
}

package exercises.dataprocessing

trait MonoFoldParam[A] {
  def default: A
  def combine(first: A, second: A): A
}

object MonoFoldParam {
  val sumInt: MonoFoldParam[Int] = new MonoFoldParam[Int] {
    override def default: Int = 0

    override def combine(first: Int, second: Int): Int = first + second
  }
}

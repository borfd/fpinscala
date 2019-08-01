sealed trait Option[+A] {
  def filter(f: A => Boolean): Option[A] = ???

  def map[B](f: A => B): Option[B] = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def getOrElse[B >: A](default: => B): B = ???

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](xs: Seq[Option[A]]): Option[Seq[A]] = ???
}

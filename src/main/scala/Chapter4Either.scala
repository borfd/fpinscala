object Chapter4Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l@Left(_) => l
      case Right(everythingIsFine) => f(everythingIsFine)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case r@Right(_) => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      (this, b) match {
        case (Right(a), Right(b)) => Right(f(a, b))
      }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]]

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = Left(value)

    override def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

    override def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    override def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

    override def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???
  }

}
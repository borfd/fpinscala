import Chapter4Either.{Left, Right}

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
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }
  }

  object Either {
//    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    //      val zero: Either[E, List[A]] = Right(List())
    //      es.foldLeft(zero) {
    //        case (Right(list), Right(el)) => Right(el::list)
    //        case (Right(_), Left(el)) => Left(el)
    //        case (Left(err), _) => Left(err)
    //      }.map(_.reverse)
    //    }
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {

      def _sequence(es: List[Either[E, A]], acc: Either[E, List[A]]): Either[E, List[A]] = {
        es match {
          case Right(el)::tail => _sequence(tail, acc.map(el::_))
          case Left(err)::_ => Left(err)
          case scala.Nil => acc.map(_.reverse)
        }
      }

      _sequence(es, Right(List.empty))
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      sequence(as.map(f))
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = Left(value)
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  }
}
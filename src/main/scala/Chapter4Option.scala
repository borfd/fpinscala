sealed trait Option[+A] {
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case None => None
  }

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  private def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case _ => None
    }


  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    def _traverse(acc: List[B], as: List[A], f: A => Option[B]): Option[List[B]] = {
      as match {
        case scala.Nil => Some(acc.reverse)
        case head :: tail => f(head) match {
          case Some(get) => _traverse(get :: acc, tail, f)
          case None => None
        }
      }
    }
    _traverse(List.empty, as, f)
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(identity)

}

object Chapter5Stream {

  sealed trait Stream[+A] {
    def headOption: scala.Option[A] = this match {
      case Empty => scala.None
      case Cons(h, _) => scala.Option[A](h())
    }

    def toList: List[A] = {
      this match {
        case Cons(head, tail) => head()::tail().toList
        case Empty => List.empty
      }
    }

    import Stream._
    def take(n: Int): Stream[A] = {
      this match {
        case Cons(head, tail) if n > 0 => cons(head(), tail().take(n - 1))
        case _ => Stream.empty
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Cons(head, tail) if n > 0 => tail().drop(n - 1)
        case _ if n == 0 => this
        case _ => Stream.empty
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      this match {
        case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
        case _ => Stream.empty
      }
    }


    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // Checks if all the elements in the stream match a given predicate
    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((el, acc) => p(el) && acc)
    }

    //Implement using foldRight
    def takeWhile2(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((el, acc) => {
        if (p(el)) {
          cons(el, acc)
        } else {
          Stream.empty[A]
        }
      })
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((el, acc) => cons(f(el),acc))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(empty[A])((h,t) =>
        if (f(h)) cons(h, t)
        else t)
    }

    def append[B >: A](el: => B): Stream[B] = this match {
      case Empty => cons(el, Stream.empty[B])
      case Cons(head, tail) => cons(head(), tail().append(el))
    }

  }
  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl

      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

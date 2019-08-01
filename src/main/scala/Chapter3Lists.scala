import scala.annotation.tailrec

sealed trait List[+A]

object List {
  def apply[A](as: A*): List[A] = {
    as.foldRight[List[A]](Nil)(Cons.apply)
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Chapter3Lists {

  val listOfOneArgumentWhichIsAfive: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(_, tail) => tail
    case Nil => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n == 0 => l
    case Cons(head, rest) =>
      drop(rest, n - 1)
    case Nil => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def setHead[A](newHead: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(newHead, tail)
  }


  def concat[A](a1: List[A], a2: List[A]): List[A] = {
    def concatRec(a1: List[A], a2: List[A], buf: List[A]): List[A] = {
      (a1, buf) match {
        case (Nil, Nil) => a2
        case (Nil, Cons(head, tail)) => concatRec(Nil, Cons(head, a2), tail)
        case (Cons(head, tail), _) => concatRec(tail, a2, Cons(head, buf))
      }
    }

    concatRec(a1, a2, Nil)
  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def init2[A](l: List[A]): List[A] = {

    @tailrec
    def initRec(in: List[A], res: List[A]): List[A] = in match {
      case Nil | Cons(_, Nil) => reverse(res, Nil)
      case Cons(head, tail) => initRec(tail, Cons(head, res))
    }

    @tailrec
    def reverse(in: List[A], res: List[A]): List[A] = in match {
      case Nil => res
      case Cons(head, tail) => reverse(tail, Cons(head, res))
    }

    initRec(l, Nil)
  }

  // FOLD

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def foldLeftRec(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(head, tail) => foldLeftRec(tail, f(acc, head))
    }

    foldLeftRec(as, z)
  }

  def lengthLeftFold[A](as: List[A]): Int = foldLeft(as, 0) { case (acc, a) => acc + 1 }

  def sum(as: List[Int]): Int = foldLeft(as, 0) {
    _ + _
  }

  def lengthFoldRight[A](as: List[A]): Int = ???

  def sumFoldLeft(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  private def reverse[A](in: List[A], res: List[A]): List[A] = in match {
    case Nil => res
    case Cons(head, tail) => reverse(tail, Cons(head, res))
  }

  def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as, Nil), z) { case (a, b) => f(b, a) }
  }

  def foldLeftWithFoldRightChain[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, identity[B](_)) { case (a, fb) => (f(_, a)).andThen(fb) }(z)
  }

  def foldRightWithFoldLeftChain[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as, Nil), z) { case (b, a) => f(a, b) }
  }

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, { b: B => b }) { case (fb, a) => fb.andThen(f(a, _)) }(z)
  }

  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1, Nil), a2) { case (acc, a) => Cons(a, acc) }
  }

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2) { case (a, acc) => Cons(a, acc) }
  }

  def flatten[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, Nil: List[A])(concat)
  }

  // MAP + FILTER

  def listPlusOne(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int]) { case (a, acc) => Cons(a + 1, acc) }
  }

  def listDoubleToString(as: List[Double]): List[String] = {
    foldRight(as, Nil: List[String]) { case (a, acc) => Cons(a.toString, acc) }
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B]) { case (a, acc) => Cons(f(a), acc) }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) {
      case (a, acc) if f(a) => Cons(a, acc)
      case (_, acc) => acc
    }


  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B]) { case (a: A, acc: List[B]) =>
      concat(f(a), acc)
    }
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) { a =>
      if (f(a)) Cons(a, Nil)
      else Nil
    }
  }

  // ZIPS

  def addTwoLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons(a + b, addTwoLists(tailA, tailB))
    case _ => Nil
  }

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons(f(a, b), zipWith(tailA, tailB)(f))
    case _ => Nil
  }

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith(as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
      case (_, Nil) => true
      case (Cons(a, tailA), Cons(p, tailP)) if a == p => startsWith(tailA, tailP)
      case _ => false
    }

    list match {
      case _ if startsWith(list, sub) => true
      case Cons(a, tail) => startsWith(tail, sub)
      case _ => false
    }
  }
}

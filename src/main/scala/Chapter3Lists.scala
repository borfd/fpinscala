import scala.annotation.tailrec

sealed trait OurList[+A]

object OurList {
  def apply[A](as: A*): OurList[A] = {
    as.foldRight[OurList[A]](Nil)(Cons.apply)
  }
}

case object Nil extends OurList[Nothing]

case class Cons[+A](head: A, tail: OurList[A]) extends OurList[A]

object Chapter3Lists {

  val listOfOneArgumentWhichIsAfive: OurList[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def tail[A](xs: OurList[A]): OurList[A] = xs match {
    case Cons(_, tail) => tail
    case Nil => Nil
  }

  def drop[A](l: OurList[A], n: Int): OurList[A] = l match {
    case _ if n == 0 => l
    case Cons(head, rest) =>
      drop(rest, n - 1)
    case Nil => Nil
  }

  def dropWhile[A](l: OurList[A], f: A => Boolean): OurList[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def setHead[A](newHead: A, xs: OurList[A]): OurList[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => Cons(newHead, tail)
  }


  def concat[A](a1: OurList[A], a2: OurList[A]): OurList[A] = {
    def concatRec(a1: OurList[A], a2: OurList[A], buf: OurList[A]): OurList[A] = {
      (a1, buf) match {
        case (Nil, Nil) => a2
        case (Nil, Cons(head, tail)) => concatRec(Nil, Cons(head, a2), tail)
        case (Cons(head, tail), _) => concatRec(tail, a2, Cons(head, buf))
      }
    }

    concatRec(a1, a2, Nil)
  }


  def init[A](l: OurList[A]): OurList[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def init2[A](l: OurList[A]): OurList[A] = {

    @tailrec
    def initRec(in: OurList[A], res: OurList[A]): OurList[A] = in match {
      case Nil | Cons(_, Nil) => reverse(res, Nil)
      case Cons(head, tail) => initRec(tail, Cons(head, res))
    }

    @tailrec
    def reverse(in: OurList[A], res: OurList[A]): OurList[A] = in match {
      case Nil => res
      case Cons(head, tail) => reverse(tail, Cons(head, res))
    }

    initRec(l, Nil)
  }

  // FOLD

  def foldRight[A, B](as: OurList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: OurList[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def foldLeftRec(as: OurList[A], acc: B): B = as match {
      case Nil => acc
      case Cons(head, tail) => foldLeftRec(tail, f(acc, head))
    }

    foldLeftRec(as, z)
  }

  def lengthLeftFold[A](as: OurList[A]): Int = foldLeft(as, 0) { case (acc, a) => acc + 1 }

  def sum(as: OurList[Int]): Int = foldLeft(as, 0) {
    _ + _
  }

  def lengthFoldRight[A](as: OurList[A]): Int = ???

  def sumFoldLeft(as: OurList[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  private def reverse[A](in: OurList[A], res: OurList[A]): OurList[A] = in match {
    case Nil => res
    case Cons(head, tail) => reverse(tail, Cons(head, res))
  }

  def foldLeftWithFoldRight[A, B](as: OurList[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as, Nil), z) { case (a, b) => f(b, a) }
  }

  def foldLeftWithFoldRightChain[A, B](as: OurList[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, identity[B](_)) { case (a, fb) => (f(_, a)).andThen(fb) }(z)
  }

  def foldRightWithFoldLeftChain[A, B](as: OurList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as, Nil), z) { case (b, a) => f(a, b) }
  }

  def foldRightWithFoldLeft[A, B](as: OurList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, { b: B => b }) { case (fb, a) => fb.andThen(f(a, _)) }(z)
  }

  def appendFoldLeft[A](a1: OurList[A], a2: OurList[A]): OurList[A] = {
    foldLeft(reverse(a1, Nil), a2) { case (acc, a) => Cons(a, acc) }
  }

  def appendFoldRight[A](a1: OurList[A], a2: OurList[A]): OurList[A] = {
    foldRight(a1, a2) { case (a, acc) => Cons(a, acc) }
  }

  def flatten[A](lists: OurList[OurList[A]]): OurList[A] = {
    foldRight(lists, Nil: OurList[A])(concat)
  }

  // MAP + FILTER

  def listPlusOne(as: OurList[Int]): OurList[Int] = {
    foldRight(as, Nil: OurList[Int]) { case (a, acc) => Cons(a + 1, acc) }
  }

  def listDoubleToString(as: OurList[Double]): OurList[String] = {
    foldRight(as, Nil: OurList[String]) { case (a, acc) => Cons(a.toString, acc) }
  }

  def map[A, B](as: OurList[A])(f: A => B): OurList[B] = {
    foldRight(as, Nil: OurList[B]) { case (a, acc) => Cons(f(a), acc) }
  }

  def filter[A](as: OurList[A])(f: A => Boolean): OurList[A] =
    foldRight(as, Nil: OurList[A]) {
      case (a, acc) if f(a) => Cons(a, acc)
      case (_, acc) => acc
    }


  def flatMap[A, B](as: OurList[A])(f: A => OurList[B]): OurList[B] = {
    foldRight(as, Nil: OurList[B]) { case (a: A, acc: OurList[B]) =>
      concat(f(a), acc)
    }
  }

  def filterWithFlatMap[A](as: OurList[A])(f: A => Boolean): OurList[A] = {
    flatMap(as) { a =>
      if (f(a)) Cons(a, Nil)
      else Nil
    }
  }

  // ZIPS

  def addTwoLists(as: OurList[Int], bs: OurList[Int]): OurList[Int] = (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons(a + b, addTwoLists(tailA, tailB))
    case _ => Nil
  }

  def zipWith[A](as: OurList[A], bs: OurList[A])(f: (A, A) => A): OurList[A] = (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons(f(a, b), zipWith(tailA, tailB)(f))
    case _ => Nil
  }

  def hasSubsequence[A](list: OurList[A], sub: OurList[A]): Boolean = {
    @tailrec
    def startsWith(as: OurList[A], prefix: OurList[A]): Boolean = (as, prefix) match {
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

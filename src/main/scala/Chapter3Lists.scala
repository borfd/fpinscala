sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Chapter3Lists {

  def tail[A](xs: List[A]): List[A] = ???

  def setHead[A](newHead: A, xs: List[A]): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // FOLD

  def length[A](as: List[A]): Int = ???

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

  def foldLeftWithFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???

  // MAP + FILTER

  def listPlusOne(as: List[Int]): List[Int] = ???

  def listDoubleToString(as: List[Double]): List[String] = ???

  def map[A,B](as: List[A])(f: A => B): List[B] = ???

  def filter[A](as: List[A])(f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = ???

  // ZIPS

  def addTwoLists[A](as: List[A], bs: List[A]): List[A] = ???

  def zipWith[A](as: List[A], bs: List[A]): List[A] = ???

}

import Tree._

sealed trait Tree[+A]

object Tree {
  case class Leaf[+A](value: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
}


object Chapter3Trees {

  def countLeaf[A](res: Int, stack: OurList[Tree[A]]): Int = {

    stack match {
      case Nil => res
      case Cons(Leaf(_), tail) => countLeaf(res + 1, tail)
      case Cons(Branch(l, r), tail) => countLeaf(res, Cons(l, Cons(r, tail)))
    }
  }


  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => Math.max(max(left), max(right))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + Math.max(depth(left), depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A], z: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(value) => z(value)
    case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f))
  }

  def maxFold(tree: Tree[Int]): Int = fold(tree, identity[Int])(Math.max)
}

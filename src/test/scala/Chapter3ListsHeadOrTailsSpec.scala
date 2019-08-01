import org.specs2.Specification

class Chapter3ListsHeadOrTailsSpec extends Specification { def is =

  s2"""
    Here we play around with implementing lists
    purely with functions.

    We investigate how to hold and modify state
    without side-effects
    in the context of normal list interactions.


    Sample list implementation from the red book:

    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    tail([3, 4, 5]) should give us only the tail [4, 5] $e1

    setHead(42, [3, 4, 5]) should give us [42, 4, 5]    $e2
  """

  def threeFourFiveList: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def e1 =
    Chapter3Lists.tail(threeFourFiveList) must equalTo(Cons(4, Cons(5, Nil)))

  def e2 =
    Chapter3Lists.setHead(42, threeFourFiveList) must equalTo(Cons(42, (Cons(4, Cons(5, Nil)))))
}

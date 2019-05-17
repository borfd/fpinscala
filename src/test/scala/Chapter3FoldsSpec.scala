import org.specs2.Specification

class Chapter3FoldsSpec extends Specification { def is =
  s2"""
    Generalizing away duplication by
    pulling subexpressions into function arguments
    is how functional composition grows
    organically.

    Here we will cover Folds which generally
    can be thought of functions which accept a List
    and return a single result.

    @TODO: Let us start with #length, the simplest one
    which should give us the size of the list:                    $e1

    @TODO: Given the implementation of #foldRight,
    implement a tail-recursive version of #foldLeft
    using the techniques we discussed in the
    previous chapter.

    foldLeft([3, 4, 5], 0)(sum) should equalTo 10                 $e2
    foldLeft([3, 4, 5], 10)(sum) should equalTo 20                $e3

    Furthermore implement #foldRight using #foldLeft
    and #foldLeft using #foldRight :troll:

    foldLeftWithFoldRight([3, 4, 5], 0)(sum) should equalTo 10    $e4

    foldRightWithFoldLeft([3, 4, 5], 0)(sum) should equalTo 10    $e5
  """

  def threeFourFiveList: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def e1 =
    Chapter3Lists.length(threeFourFiveList) must equalTo(3)

  def e2 =
    Chapter3Lists.foldLeft(threeFourFiveList, 0)((acc, curr) => acc + curr) must equalTo(10)

  def e3 =
    Chapter3Lists.foldLeft(threeFourFiveList, 10)((acc, curr) => acc + curr) must equalTo(20)

  def e4 =
    Chapter3Lists.foldLeftWithFoldRight(threeFourFiveList, 10)((acc, curr) => acc + curr) must equalTo(10)

  def e4 =
    Chapter3Lists.foldRightWithFoldLeft(threeFourFiveList, 10)((acc, curr) => acc + curr) must equalTo(10)

}

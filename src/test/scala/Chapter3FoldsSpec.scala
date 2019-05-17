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
    which should give us the size of the list:          $e1
  """

  def threeFourFiveList: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def e1 = Chapter3Lists.length(threeFourFiveList) must equalTo(3)

}

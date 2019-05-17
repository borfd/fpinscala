import org.specs2.Specification

class Chapter2ComposeSpec extends Specification { def is =
  s2"""

  Function composing feeds the output of one function
  to the input of another function.

  In more concrete terms:

  It is an operation that takes two functions f and g
  and produces a function h such that h(x) = g(f(x)).

  @TODO: implement #compose

  compose(f, g)(0) == compose(g, f)(0) $e1
  compose(f, g)(2) shouldBe 2          $e2
  compose(g, f)(2) shouldBe 2          $e3

  """

  def f(b: Int): Int = b / 2
  def g(a: Int): Int = a + 2

  def e1 = Chapter2Curry.compose(f, g)(0) must not equalTo Chapter2Curry.compose(g, f)(0)

  def e2 = Chapter2Curry.compose(f, g)(2) must equalTo(2)

  def e3 = Chapter2Curry.compose(g, f)(2) must equalTo(3)
}

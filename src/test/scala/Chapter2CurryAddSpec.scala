import org.specs2.Specification

class Chapter2CurryAddSpec extends Specification { def is =
  s2"""

      In mathematics and computer science,
      currying is the technique of translating the evaluation of a function
      that takes multiple arguments (or a tuple of arguments)
      into evaluating a sequence of functions,
      each with a single argument (partial application).

      So by definition, a function add(a,b,c) is a curried function

      @TODO: Implement #curry as such:

      curry(f)(1)(1) should be the same as f(1, 1) $e1
      curry(f)(1)(1) should be the same as g(1)(1) $e2
      curry(f)(1)(1) should equalTo 2              $e3
      f(1, 1)        should equalTo 2              $e4
    """


  def f(a: Int, b: Int): Int = a + b

  def g(a: Int)(b: Int): Int = a + b

  def e1 = Chapter2Curry.curry(f)(1)(1) must equalTo(f(1, 1))

  def e2 = Chapter2Curry.curry(f)(1)(1) must equalTo(g(1)(1))

  def e3 = Chapter2Curry.curry(f)(1)(1) must equalTo(2)

  def e4 = Chapter2Curry.curry(f)(1)(1) must equalTo(2)

}

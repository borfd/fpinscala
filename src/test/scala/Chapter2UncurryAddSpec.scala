import org.specs2.Specification

class Chapter2UncurryAddTest extends Specification {def is =

    s2"""

      In mathematics and computer science,
      currying is the technique of translating the evaluation of a function
      that takes multiple arguments (or a tuple of arguments)
      into evaluating a sequence of functions,
      each with a single argument (partial application).


      Let's test uncurrying
      which is the reverse transformation of curry.

      uncurry(g)(1, 1) should be the same as g(1)(1) $e1
      uncurry(g)(1, 1) should be the same as f(1, 1) $e2

    """


    def f(a: Int, b: Int): Int = a + b

    def g(a: Int)(b: Int): Int = a + b

    def e1 = Chapter2Curry.uncurry(g)(1, 1) must equalTo(g(1)(1))

    def e2 = Chapter2Curry.uncurry(g)(1, 1) must equalTo(f(1, 1))


}

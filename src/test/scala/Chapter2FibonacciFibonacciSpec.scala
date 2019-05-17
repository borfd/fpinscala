import org.specs2.Specification
import org.specs2.mutable.Tables

class Chapter2FibonacciFibonacciSpec extends Specification with Tables {
  def is =
    s2"""

 One of the first concepts one learns when starting with FP
 is recursion. Recursion allows us to iterate over collections
 without mutation and allocations.

 We find ourselves having to
 write two functions when implementing recursion:
 - one to kick off the computation
 - and one which has an extra argument which actually implements
 the recursive call

 Conveniently, in Scala, we can define a function within a
 function and encapsulate this internal behaviour and hide
 it from callers.

 Here is an example of such a recursive function:


    def factorial(n: Int): Int = {
      def go(n: Int, acc: Int): Int =
        if (n <= 0) acc else go(n-1, n*acc)
        go(n, 1)
    }


 This is a specification to check our implementation
 of a recursive function by computing the Fibonacci sequence.

 @TODO: Implement #fib

 The recursive Fibonacci function must work ${
      "input" | "result" |>
        0     !    0     |
        1     !    1     |
        2     !    1     |
        3     !    2     |
        4     !    3     |
        5     !    5     |
        6     !    8     |
        7     !    13    |
        { (input, result) => Chapter2Fibonacci.fib(input) must equalTo(result) }
    }
  """


}

import org.specs2.Specification
import org.specs2.mutable.Tables

class Chapter2IsSortedSpec extends Specification with Tables { def is =

    s2"""

      @TODO: implement a function #isSorted
      which indicates whether the input is sorted

      def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean


      isSorted should work ${
      "input"           | "result" |>
       Array(0, 1)      !  true    |
       Array(1, 0)      !  false   |
       Array(1,2,3,4,3) !  true    |
      { (input, result) =>
        Chapter2IsSorted.isSorted(input, (x: Int, y: Int) => x > y) must equalTo(result)
      }
    }
    """
}

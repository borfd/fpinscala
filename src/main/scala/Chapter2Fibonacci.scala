import scala.annotation.tailrec

object Chapter2Fibonacci {
  def fib(i: Int): Int = {

    @tailrec
    def go(i: Int, current: Int, next: Int): Int = {
      if(i == 0) {
        return current
      }

      go(i - 1, next, current + next)
    }
    go(i, 0, 1)
  }
}

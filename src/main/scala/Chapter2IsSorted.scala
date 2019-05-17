import scala.annotation.tailrec

object Chapter2IsSorted {

  def isSorted[A](as: Array[A], sorted: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(index: Int): Boolean = {
      if (index >= as.size - 1) return true
      else if (!sorted(as(index), as(index+1))) {
        return false
      }
      go(index + 1)
    }
    go(0)
  }

  // 0 1 2 3
  // sorted(0, 1)

}

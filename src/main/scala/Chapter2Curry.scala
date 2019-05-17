object Chapter2Curry {

  def curry(f: (Int, String) => Long): Int => (String => Long) =
    (a: Int) => (b: String) => f(a, b)

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)

  def compose[A, B, C](f: B => C, g: A => B): A => C = ???

}

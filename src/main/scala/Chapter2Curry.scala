object Chapter2Curry {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = ???

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???

  def compose[A, B, C](f: B => C, g: A => B): A => C = ???

}

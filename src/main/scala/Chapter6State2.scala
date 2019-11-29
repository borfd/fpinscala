import Chapter6State1._

object Chapter6State2 {

  case class State[S, +A](run: S => (A, S))

  type Rand[A] = State[RNG, A]

  // 6.10
  /*
  Generalize the functions unit, map, map2, flatMap, and sequence.
  Add them as meth- ods on the State case class where possible.
  Otherwise you should put them in a State companion object.
   */

  //def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = ???

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // 6.11
  /*
  Hard: To gain experience with the use of State, implement a finite state
  automaton that models a simple candy dispenser. The machine has two types
  of input: you can insert a coin, or you can turn the knob to dispense
  candy. It can be in one of two states: locked or unlocked. It also tracks
  how many candies are left and how many coins it contains.
   */

  // RULES
  /*
  The rules of the machine are as follows:
 Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
 Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 A machine that’s out of candy ignores all inputs.
   */

  /*
  The method simulateMachine should operate the machine based on the list of inputs and
  return the number of coins and candies left in the machine at the end. For exam- ple,
  if the input Machine has 10 coins and 5 candies, and a total of 4 candies are suc-
  cessfully bought, the output should be (14, 1).
   */

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}

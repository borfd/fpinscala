import Chapter6State1._


object Chapter6State2 {

  case class State[S, +A](run: S => (A, S)) {
    // 6.11
    /*
    Generalize the functions unit, map, map2, flatMap, and sequence.
    Add them as meth- ods on the State case class where possible.
    Otherwise you should put them in a State companion object.
     */
    def map[B](f: A => B): State[S, B] = {
      State { state =>
        val (a, newState) = this.run(state)
        (f(a), newState)
      }
    }

    def map2[B, C](fb: State[S, B])(f: (A, B) => C): State[S, C] = {
      State { state =>
        val (a, newState1) = this.run(state)
        val (b, newState2) = fb.run(newState1)
        (f(a, b), newState2)
      }
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State { state =>
        val (a, newState1) = this.run(state)
        val fb: State[S, B] = g(a)
        val (b, newState2) = fb.run(newState1)
        (b, newState2)
      }
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = {
      State(state => (a, state))
    }

    def sequence[S, A](seq: List[State[S, A]]): State[S, List[A]] = {
      seq.foldLeft(State.unit[S, List[A]](scala.Nil)) { case (accS, nextS) =>
        for {
          acc <- accS
          nextA <- nextS
        } yield nextA :: acc
      }.map(_.reverse)
    }

    /*
    6.12: Come up with the signatures for get and set, then write their implementations.
     */

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }


  /*
  6.13 (hard): To gain experience with the use of State, implement a finite state
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

  private def handleInput(input: Input): State[Machine, Unit] = {
    for {
      machine <- State.get[Machine]
      _ <- State.set {
        (input, machine) match {
          case (Coin, _) if machine.locked && machine.candies > 0 =>
            machine.copy(locked = false, coins = machine.coins + 1)
          case (Turn, _) if !machine.locked && machine.candies > 0 =>
            machine.copy(locked = true, candies = machine.candies - 1)
          case _ => machine
        }
      }
    } yield ()
  }
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
    for {
      _ <- State.sequence(inputs.map(handleInput))
      machine <- State.get
    } yield machine.coins
  }

}

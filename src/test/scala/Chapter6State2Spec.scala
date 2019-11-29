import Chapter6State1.{RNG, SimpleRNG}
import Chapter6State2.{Coin, Machine, Rand, State, Turn, simulateMachine, flatMap => stateFlatMap, map => stateMap, map2 => stateMap2}
import org.specs2.ScalaCheck
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class Chapter6State2Spec extends Specification with ScalaCheck with Mockito {

  /*
  def nonNegativeInt: State[RNG, Int] = {
    rng: RNG =>
      val (int, nextRng) = rng.nextInt
      int match {
        case Int.MinValue => _ => (0, nextRng)
        case _ => _ => (math.abs(int), nextRng)
      }
  }

  "map" >> {

  def plusTwo(a: Int): Int = a + 2
    "should return the same result for same input" >> {
      stateMap[RNG, Int, Int](nonNegativeInt.run)(plusTwo)(SimpleRNG(1L)) mustEqual stateMap[RNG, Int, Int](nonNegativeInt.run)(plusTwo)(SimpleRNG(1L))
    }

    "should return different result for consecutive rngs" >> {
      val rng = SimpleRNG(1L)
      val result = stateMap[RNG, Int, Int](nonNegativeInt.run)(plusTwo)(rng)
      result must_!== stateMap[RNG, Int, Int](nonNegativeInt.run)(plusTwo)(result._2)
    }

    "should apply the function correctly" >> {
      val rng = SimpleRNG(1L)
      val result = stateMap[RNG, Int, Int](nonNegativeInt.run)(plusTwo)(rng)
      val (int, nextRng) = nonNegativeInt.run(rng)
      result._1 must_=== int + 2
    }
  }

  "map2" >> {

    def addition(a: Int, b: Int): Int = a + b
    "should return the same result for same input" >> {
      val rng = SimpleRNG(1L)
      val result1 = stateMap2[Int, Int, Int](nonNegativeInt, nonNegativeInt)(addition).run(rng)
      val result2 = stateMap2[Int, Int, Int](nonNegativeInt, nonNegativeInt)(addition).run(rng)
      result1 mustEqual result2
    }

    "should return different result for consecutive rngs" >> {
      val rng = SimpleRNG(1L)
      val result1 = stateMap2[Int, Int, Int](nonNegativeInt, nonNegativeInt)(addition).run(rng)
      val result2 = stateMap2[Int, Int, Int](nonNegativeInt, nonNegativeInt)(addition).run(rng.nextInt._2)
      result1 must_!== result2
    }

    "should apply the function correctly" >> {
      val rng = SimpleRNG(1L)
      val result1 = stateMap2[Int, Int, Int](nonNegativeInt, nonNegativeInt)(addition).run(rng)
      val resultInt = nonNegativeInt.run(rng)._1
      result1._1 mustEqual resultInt + resultInt
    }
  }


  "flatMap" >> {

  }



  "simulateMachine" >> {
    "input for a machine without candy will not alter the state of the machine" >> {
      val candy = 0
      val coins = 8

      val lockedEmptyMachine = Machine(true, candy, coins)
      val unlockedEmptyMachine = Machine(false, candy, coins)

      val actions = List(Coin, Turn, Turn)

      simulateMachine(actions).run(lockedEmptyMachine)._2 mustEqual lockedEmptyMachine
      simulateMachine(List(Turn)).run(lockedEmptyMachine)._2 mustEqual lockedEmptyMachine
      simulateMachine(List()).run(lockedEmptyMachine)._2 mustEqual lockedEmptyMachine


      simulateMachine(actions).run(unlockedEmptyMachine)._2 mustEqual unlockedEmptyMachine
      simulateMachine(List(Turn)).run(unlockedEmptyMachine)._2 mustEqual unlockedEmptyMachine
      simulateMachine(List()).run(unlockedEmptyMachine)._2 mustEqual unlockedEmptyMachine
    }

    "inserting a coin into a locked machine with candy will unlock it" >> {
      val coins = 3
      val candy = 10
      val lockedMachine = Machine(true, candy, coins)
      simulateMachine(List(Coin)).run(lockedMachine) mustEqual(Machine(false, candy, coins + 1), (candy, coins + 1))
      simulateMachine(List(Coin, Coin)).run(lockedMachine) mustEqual(Machine(false, candy, coins + 2), (candy, coins + 2))
    }

    "turning the knob on an unlocked machine will cause it to dispense candy and become locked" >> {
      val coins = 3
      val candy = 10
      val unlocked = Machine(false, candy, coins)
      simulateMachine(List(Turn)).run(unlocked) mustEqual(Machine(true, candy - 1, coins), (candy - 1, coins))
      simulateMachine(List(Turn, Turn)).run(unlocked) mustEqual(Machine(true, candy - 1, coins), (candy - 1, coins))
    }

    "turning the knob on a locked machine will do nothing" >> {
      val coins = 3
      val candy = 10
      val lockedMachine = Machine(true, candy, coins)
      simulateMachine(List(Turn)).run(lockedMachine)._2 mustEqual lockedMachine
    }

    "inserting a coin into an unlocked machine does nothing" >> {
      val coins = 3
      val candy = 10
      val unlocked = Machine(false, candy, coins)
      simulateMachine(List(Coin)).run(unlocked)._2 mustEqual unlocked
    }

    "for a machine with 10 coins and 5 candies, and a total of 4 candies are bought, the output should be (14, 1)" >> {
      val coins = 10
      val candy = 5
      val locked = Machine(true, candy, coins)
      val actions = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      simulateMachine(actions).run(locked)._1 mustEqual(14, 1)
    }
  }

*/
}

import Chapter6State1._
import Chapter6State2._
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class Chapter6State2Spec extends Specification with ScalaCheck with Mockito {

  implicit class RandOps[A](rnd: Rand[A]) {
    def toState: State[RNG, A] = {
      State { rng =>
        rnd(rng)
      }
    }
  }

  "unit" >> {

    "should always return the same value" >> {
      Prop.forAll { state: Long =>
        val const = "42"
        State.unit(const).run(state)._1 ==== const
      }
    }
  }


  "map" >> {

    def plusTwo(a: Int): Int = a + 2

    "should return the same result for same input" >> {
      int.toState.map(plusTwo).run(SimpleRNG(1L)) mustEqual int.toState.map(plusTwo).run(SimpleRNG(1L))
    }

    "should return different result for consecutive rngs" >> {
      val rng = SimpleRNG(1L)
      val result = int.toState.map(plusTwo).run(rng)
      result must_!== int.toState.map(plusTwo).run(result._2)
    }

    "should apply the function correctly" >> {
      val rng = SimpleRNG(1L)
      val result = int.toState.map(plusTwo).run(rng)
      val (i, _) = int.toState.run(rng)
      result._1 mustEqual i + 2
    }
  }


  "map2" >> {
    def addition: (Int, Int) => Int = (a: Int, b: Int) => a + b

    "should return the same result for same input" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        (nonNegativeEven.toState map2 nonNegativeEven.toState) (addition).run(rng) ====
          (nonNegativeEven.toState map2 nonNegativeEven.toState) (addition).run(rng)
      }
    }

    "should return different result for consecutive rngs" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        (nonNegativeEven.toState map2 int.toState) (addition).run(rng) !===
          (nonNegativeEven.toState map2 int.toState) (addition).run(rng.nextInt._2)
      }
    }

    "should apply the function correctly" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        (nonNegativeEven.toState map2 nonNegativeEven.toState) (addition).run(rng)._1 ====
          nonNegativeEven(rng.nextInt._2)._1 + nonNegativeEven(rng)._1
      }
    }
  }

  "flatMap" >> {
    def g(intValue: Int): Rand[String] =
      (r: RNG) => (intValue.toString, r)

    "returns same output for same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        val rnd = for {
          i <- int.toState
          res <- g(i).toState
        } yield res
        rnd.run(rng) ==== rnd.run(rng)
      }
    }
    "should return different output for consecutive seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        val rng2 = rng.nextInt._2
        val rnd = for {
          i <- int.toState
          res <- g(i).toState
        } yield res
        rnd.run(rng) !=== rnd.run(rng2)
      }
    }
    "apply function correctly" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        val rnd = for {
          i <- int.toState
          res <- g(i).toState
        } yield res

        rnd.run(rng)._1 ==== int.toState.run(rng)._1.toString
      }
    }
  }


  "sequence" >> {
    "should handle empty list" >> {
      Prop.forAll { state: Long =>
        val states: List[State[Long, Int]] = scala.Nil
        State.sequence(states).run(state)._1 ==== scala.Nil
      }
    }

    "should unwrap values from list " >> {
      Prop.forAll { long1: Long =>
        val rng1 = SimpleRNG(long1)
        val rng2 = rng1.nextInt._2
        val states: List[State[RNG, Int]] = List(nonNegativeEven.toState, int.toState)
        State.sequence(states).run(rng1)._1 ==== List(nonNegativeEven(rng1)._1, int(rng2)._1)
      }
    }
  }

  "get" >> {
    "should return the current state" >> {
      Prop.forAll { state: Long =>
        val s: State[Long, Long] = for {
          s <- State.get
        } yield s

        s.run(state)._1 ==== state
      }
    }

    "should return the state that set by set" >> {
      Prop.forAll { state: Long =>
        val s: State[Long, Long] = for {
          _ <- State.set(42L)
          s <- State.get
        } yield s

        s.run(state)._1 ==== 42
      }
    }
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
      simulateMachine(List(Coin)).run(lockedMachine) mustEqual (coins + 1, Machine(false, candy, coins + 1))
    }
    "turning the knob on an unlocked machine will cause it to dispense candy and become locked" >> {
      val coins = 3
      val candy = 10
      val unlocked = Machine(false, candy, coins)
      simulateMachine(List(Turn)).run(unlocked) mustEqual(coins, Machine(true, candy - 1, coins))
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
      simulateMachine(actions).run(locked)._1 mustEqual 14
    }
  }
}

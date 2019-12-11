import Chapter6State1._
import Chapter6State2.State
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
}

import Chapter6PurelyFunctionalState._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class Chapter6PurelyFunctionalStateSpec extends Specification with ScalaCheck with Mockito {

  val minValueRng: RNG = smartMock[RNG]
  minValueRng.nextInt returns ((Int.MinValue, SimpleRNG(1L)))

  val maxValueRng: RNG = smartMock[RNG]
  maxValueRng.nextInt returns ((Int.MaxValue, SimpleRNG(2L)))

  "nonNegativeInt" >> {
    "should return the same result for same seed" >> {
      Prop.forAll { long: Long =>
        val rngArb = SimpleRNG(long)
        nonNegativeInt(rngArb) must_=== nonNegativeInt(rngArb)
      }
    }
    "should return different results for two consecutive calls" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        val tuple1 = nonNegativeInt(rng)
        val tuple2 = nonNegativeInt(tuple1._2)
        tuple1 must_!== tuple2
      }
    }
    "should return non-negative value" >> {
      Prop.forAll { long: Long =>
        nonNegativeInt(SimpleRNG(long))._1 >= 0
      }
    }
    "rng of Int.MinValue should return a new rng" >> {
      val minValueResult = nonNegativeInt(minValueRng)
      minValueResult._2 must_!== minValueRng
    }
    "rng of Int.MinValue should be non-negative" >> {
      val minValueResult = nonNegativeInt(minValueRng)
      minValueResult._1 >= 0
    }
  }

  /*
  "double" >> {
    "should return a value between 0 and < 1" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        double(rng)._1 >= 0 && double(rng)._1 < 1
      }
    }
    "should return the same values for the same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        double(rng) must_=== double(rng)
      }
    }
    "should return different values for consecutive seeds" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        double(rng) must_!== double(rng.nextInt._2)
      }
    }
    "should handle corner cases correctly" >> {
      double(minValueRng)._1 >= 0 &&
        double(minValueRng)._1 < 1 &&
        double(maxValueRng)._1 >= 0 &&
        double(maxValueRng)._1 < 1
    }
  }

  "intDouble" >> {
    "should return the same values for the same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        intDouble(rng) must_=== intDouble(rng)
      }
    }

    "should return different values for consecutive seeds" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        intDouble(rng) must_!== intDouble(rng.nextInt._2)
      }
    }
  }

  "doubleInt" >> {
    "should return the same values for the same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        doubleInt(rng) must_=== doubleInt(rng)
      }
    }

    "should return different values for consecutive seeds" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        doubleInt(rng) must_!== doubleInt(rng.nextInt._2)
      }
    }
  }

  "double3" >> {
    "should return the same values for the same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        double3(rng) must_=== double3(rng)
      }
    }

    "should return different values for consecutive seeds" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        double3(rng) must_!== double3(rng.nextInt._2)
      }
    }
  }

  "ints" >> {
    "should return the same list of ints for the same seed" >> {
      Prop.forAll { (count: Int, long: Long) =>
        val rng = SimpleRNG(long)
        ints(count)(rng) must_=== ints(count)(rng)
      }
    }
    "should return different lists for consecutive rngs" >> {
      Prop.forAll { (count: Int, long: Long) =>
        val rng = SimpleRNG(long)
        ints(count)(rng) must_!== ints(count)(rng.nextInt._2)
      }
    }
  }

  "doubleMap" >> {
    "should return a value between 0 and < 1" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        doubleMap(rng)._1 >= 0 && doubleMap(rng)._1 < 1
      }
    }
    "should return the same values for the same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        doubleMap(rng) must_=== doubleMap(rng)
      }
    }
    "should return different values for consecutive seeds" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        doubleMap(rng) must_!== doubleMap(rng.nextInt._2)
      }
    }
  }

  "map2" >> {
    def addition: (Int, Int) => Int = (a: Int, b: Int) => a + b

    "should return the same result for same input" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        map2(nonNegativeEven, nonNegativeEven)(addition)(rng) must_===
          map2(nonNegativeEven, nonNegativeEven)(addition)(rng)
        map2(nonNegativeEven, int)(addition)(rng) must_=== map2(nonNegativeEven, int)(addition)(rng)
      }
    }

    "should return different result for consecutive rngs" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        map2(nonNegativeEven, int)(addition)(rng) must_!==
          map2(nonNegativeEven, int)(addition)(rng.nextInt._2)
      }
    }

    "should apply the function correctly" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)

        map2(nonNegativeEven, nonNegativeEven)(addition)(rng)._1 must_=== 2 * nonNegativeEven(rng)._1
        map2(nonNegativeEven, int)(addition)(rng)._1 must_=== nonNegativeEven(rng)._1 + int(rng)._1
      }
    }
  }

  "sequence" >> {
    "should compute the same output for same input" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)
        val list = List(nonNegativeEven, int)

        sequence(list)(rng) must_=== sequence(list)(rng)
      }
    }

    "should compute different outpot for consecutive rngs" >> {
      Prop.forAll { long1: Long =>
        val rng = SimpleRNG(long1)
        val list = List(nonNegativeEven, int)

        sequence(list)(rng) must_!== sequence(list)(rng.nextInt._2)
      }
    }
  }

  "intsSeq" >> {
    "should return the same list of ints for the same seed" >> {
      Prop.forAll { (count: Int, long: Long) =>
        val rng = SimpleRNG(long)
        intsSeq(count)(rng) must_=== intsSeq(count)(rng)
      }
    }
    "should return different lists for consecutive rngs" >> {
      Prop.forAll { (count: Int, long: Long) =>
        val rng = SimpleRNG(long)
        intsSeq(count)(rng) must_!== intsSeq(count)(rng.nextInt._2)
      }
    }
  }

  "flatMap" >> {
    def g(intValue: Int): Rand[String] =
      (r: RNG) => (intValue.toString, r)

    "returns same output for same seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        flatMap[Int, String](int)(g)(rng) must_=== flatMap[Int, String](int)(g)(rng)
      }
    }
    "should return different output for consecutive seed" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        flatMap[Int, String](int)(g)(rng) must_!== flatMap[Int, String](int)(g)(rng.nextInt._2)
      }
    }
    "apply function correctly" >> {
      Prop.forAll { long: Long =>
        val rng = SimpleRNG(long)
        flatMap[Int, String](int)(g)(rng)._1 must_=== int(rng)._1.toString
      }
    }
  }

  "nonNegativeLessThan" >> {
    "should return same result for same seed" >> {
      Prop.forAll { long: Long =>
        val n = 7
        val rng = SimpleRNG(long)
        nonNegativeLessThan(n)(rng) must_=== nonNegativeLessThan(n)(rng)
      }
    }
    "should return values less than given input n (n >= 0) and positive" >> {
      Prop.forAll { long: Long =>
        val n = 7
        val rng = SimpleRNG(long)
        nonNegativeLessThan(n)(rng)._1 >= 0 &&
          nonNegativeLessThan(n)(rng)._1 < n
      }
    }
    "should return values less than given input n (n >= 0) and positive" >> {
      Prop.forAll {
        (n: Int => n, long: Long) ==> n >= 0 ===>
        val rng = SimpleRNG(long)
        nonNegativeLessThan(n)(rng)._1 >= 0 &&
          nonNegativeLessThan(n)(rng)._1 < n
      }
    }
  }
*/
}

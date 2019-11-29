import scala.annotation.tailrec

object Chapter6State1 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  // use RNG.nextInt to generate random integer between 0 and
  // Int.maxValue (inclusive)
  // handle corner case when nextInt returns Int.minValue which
  // doesn't have non-neg counter part
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    int match {
      case Int.MinValue => (0, nextRng)
      case _ => (math.abs(int), nextRng)
    }
  }

  // 6.2
  // generate Double between 0 and 1, not including 1
  // can use: Int.MaxValue, x.toDouble to convert x: Int to Double
  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    val double: Double = int.toDouble / (Int.MaxValue.toDouble + 1)
    (double, nextRng)
  }

  // 6.3
  /*
  Write functions to generate an (Int, Double) pair, a (Double, Int)
  pair, and a (Double, Double, Double) 3-tuple.
  You should be able to reuse the functions you’ve already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, nextGen1) = rng.nextInt
    val (doubleValue, nextGen2) = double(nextGen1)

    ((int, doubleValue), nextGen2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((tuple), nextGen) = intDouble(rng)
    (tuple.swap, nextGen)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val ((d1 :: d2 :: d3 :: tail), nextRng) = randN[Double](3, rng)(double)
    ((d1, d2, d3), nextRng)
  }

  @tailrec
  def randN[A](n: Int, rng: RNG, acc: List[A] = List.empty[A])(f: RNG => (A, RNG)): (List[A], RNG) = {
    if (n > 0) {
      val (nextDouble, nextRng) = f(rng)
      randN(n - 1, nextRng, nextDouble :: acc)(f)
    } else {
      (acc, rng)
    }
  }

  // 6.4
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    randN[Int](count, rng)(_.nextInt)
  }

  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  // pass state without using it
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt(_))(i => i - i % 2)

  // 6.5
  // Use map to reimplement double in a more elegant way.
  // See exercise 6.2.
  def doubleMap(rng: RNG): (Double, RNG) = ???

  // 6.6
  /*
  Write the implementation of map2 based on the following
  signature. This function takes two actions, ra and rb,
  and a function f for combining their results,
  and returns a new action that combines them:
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both(int, double)
  lazy val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7 HARD!!!!
  /*
  If you can combine two RNG transitions, you should be able to combine
  a whole list of them. Implement sequence for combining a List of
  transitions into a single transition.
  Use it to reimplement the ints function you wrote before. For the latter,
  you can use the standard library function List.fill(n)(x) to make a list with x
  repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def intsSeq(count: Int)(rng: RNG): (List[Int], RNG) = ???

  // 6.8
  /*
  flatMap allows us to generate a random A with Rand[A], and then
  take that A and choose a Rand[B] based on its value.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def nonNegativeLessThan(n: Int): Rand[Int] = ???

  /* 6.9
  Reimplement map and map2 in terms of flatMap. The fact that this
  is possible is what we’re referring to when we say that flatMap
  is more powerful than map and map2.
   */

}

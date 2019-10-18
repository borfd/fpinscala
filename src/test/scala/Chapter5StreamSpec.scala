import org.specs2.Specification
import Chapter5Stream.Stream

class Chapter5StreamSpec extends Specification {
  def is =
    s2"""
    Stream
    Stream.toList               $e1 $e2 $e3
    Stream.take                 $e4 $e5 $e6 $e7
    Stream.drop                 $e8 $e9 $e10 $e11 $e12
    Stream.takeWhile            $e13 $e14 $e15 $e16
    Stream.forAll               $e17 $e18
    Stream.takeWhile2           $e19 $e20 $e21
    Stream.map                  $e22 $e23
    Stream.filter               $e24
    Stream.append               $e25
  """

  val stream = Stream("a", "b", "c")
  val numbers = Stream(1, 3, 11, 5, 4, 1, 5)
  val nil = Stream(scala.Nil)
  val empty = Stream()


  def e1 =
    stream.toList must equalTo(List("a", "b", "c"))

  def e2 =
    nil.toList must equalTo(List(scala.Nil))

  def e3 =
    empty.toList must equalTo(List())

  def e4 =
    stream.take(2).toList must equalTo(List("a", "b"))

  def e5 =
    stream.take(7).toList must equalTo(List("a", "b", "c"))

  def e6 =
    empty.take(3).toList must equalTo(List.empty)

  def e7 =
    stream.take(0).toList must equalTo(List.empty)

  def e8 =
    stream.drop(1).toList must equalTo(List("b", "c"))

  def e9 =
    stream.drop(3).toList must equalTo(List.empty)

  def e10 =
    stream.drop(7).toList must equalTo(List.empty)

  def e11 =
    stream.drop(0).toList must equalTo(List("a", "b", "c"))

  def e12 =
    empty.drop(2).toList must equalTo(List.empty)

  def e13 =
    numbers.takeWhile(_ % 2 == 1).toList must equalTo(List(1, 3, 11, 5))

  def e14 =
    numbers.takeWhile(_ % 2 == 0).toList must equalTo(List.empty)

  def e15 =
    numbers.takeWhile(_ < 100).toList must equalTo(List(1, 3, 11, 5, 4, 1, 5))

  def e16 =
    Stream.empty[String].takeWhile(_.length > 1).toList must equalTo(List.empty)

  def e17 =
    numbers.forAll(_ % 2 == 0) must equalTo(false)

  def e18 =
    numbers.forAll(_ < 100) must equalTo(true)

  def e19 =
    numbers.takeWhile2(_ % 2 == 1).toList must equalTo(List(1, 3, 11, 5))

  def e20 =
    numbers.takeWhile2(_ % 2 == 0).toList must equalTo(List.empty)

  def e21 =
    numbers.takeWhile2(_ < 100).toList must equalTo(List(1, 3, 11, 5, 4, 1, 5))

  def e22 =
    numbers.map(_ * 2).toList must equalTo(List(2, 6, 22, 10, 8, 2, 10))

  def e23 =
    numbers.takeWhile(_ < 10).map(_ - 1).toList must equalTo(List(0, 2))

  def e24 =
    numbers.map(_ / 10).filter(_ > 0).toList must equalTo(List(1))

  def e25 =
    stream.append("d").toList must be equalTo(List("a", "b", "c", "d"))
}
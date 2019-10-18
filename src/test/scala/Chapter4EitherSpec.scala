import Chapter4Either.{Either, Left, Right}
import org.specs2.Specification

class Chapter4EitherSpec extends Specification {

  import Chapter4Either.Either._

  def is =
    s2"""
    One thing you may have noticed with Option is that
    it doesn’t tell us anything about what went wrong
    in the case of an exceptional condition.
    All it can do is give us None, indicating that there’s no value to be had.
    But sometimes we want to know more.
    For example, we might want a String that gives more information,
    or if an exception was raised, we might want to know what that error actually was.

    Either has only two cases just like Option.
    The essential difference is that both cases carry a value.

    Left.map      returns error value               $e1
    Left.flatMap  returns error value               $e3
    Left.orElse   returns value passed in           $e5
    Left.map2     returns error value (if any of the two Either is Left) $e9 $e10 $e11
    Left.sequence $e13 $e14 $e15
    Left.traverse $e17

    Right.map     applies the passed in function    $e2
    Right.flatMap applies the passed in function    $e4
    Right.orElse  returns value                     $e6
    Right.map2    applies the passed in function    $e7 $e8
    Right.sequence $e12
    Right.traverse $e16
  """

  val exception = new RuntimeException("Something is not right")

  val left = Left(exception)
  val right = Right("Everything is right")

  def e1 =
    left.map(_ => "Sd") must equalTo(Left(exception))

  def e2 =
    right.map(_ => "Want to get this back") must equalTo(Right("Want to get this back"))

  def e3 =
    left.flatMap(_ => Right("something")) must equalTo(left)

  def e4 =
    right.flatMap(s => Right(s"$s whoop whoop")) must equalTo(Right("Everything is right whoop whoop"))


  def e5 =
    left.orElse(Right("Default value")) must equalTo(Right("Default value"))

  def e6 =
    right.orElse(Right("do not use this")) must equalTo(right)

  def e7 =
    right.map2(Right("another either"))((a, b) => a + b) must equalTo(Right("Everything is rightanother either"))

  def e8 =
    right.map2(Left("something which is a left"))((a, b) => a + b) must equalTo(Left("something which is a left"))

  def e9 =
    left.map2(Right("does not matter"))((a, b) => 42) must equalTo(left)

  def e10 =
    right.map2(left)((a, b) => 42) must equalTo(left)

  def e11 =
    left.map2(Left("another left"))((a, b) => 42) must equalTo(left)

  val allRigths = List(Right("firstRight"), Right("secondRight"), Right("thirdRight"))
  val someRights = List(Right("firstRight"), Left("firstLeft"), Right("secondRight"), Left("secondLeft"))
  val allLefts = List(Left("firstLeft"), Left("secondLeft"))
  val empty = scala.Nil

  def e12 =
    sequence(allRigths) must equalTo(Right(List("firstRight", "secondRight", "thirdRight")))

  def e13 =
    sequence(someRights) must equalTo(Left("firstLeft"))

  def e14 =
    sequence(allLefts) must equalTo(Left("firstLeft"))

  def e15 =
    sequence(empty) must equalTo(Right(empty))

  def allSmallerThan11 = List(1, 4, 2)
  def hasBiggerThan11 = List(1, 17, 9, 2)

  def filterHighNumbers
    = (num: Int) => if (num > 11) Left("We don't like high numbers") else Right(num)

  def e16 =
    traverse(allSmallerThan11)(filterHighNumbers) must
      equalTo(Right(allSmallerThan11))

  def e17 =
    traverse(hasBiggerThan11)(filterHighNumbers) must equalTo(Left("We don't like high numbers"))
}

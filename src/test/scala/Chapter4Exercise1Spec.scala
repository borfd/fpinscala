import org.specs2.Specification

class Chapter4Exercise1Spec extends Specification {

  import Option._

  def is =
    s2"""
    map for addition function on None $e1
    map for addition function on Some(1) $e2

    flatMap for division function on None $flatMapNone
    flatMap for division function on Some $flatMapSome
    flatMap for division function on Some divided by zero $flatMapSomeDivideByZero

    getOrElse(10) for Some(5) should be 5 $getOrElseSome
    getOrElse(10) for None should be 10 $getOrElseNone

    orElse for Some(5) should be Some(5) $orElseSome
    orElse(Some(10)) for None should be Some(10) $orElseNone

    variance should be None for Seq.empty $e3
    variance should be Some(2.5) for Seq(1.0, 2.0, 4.0, 5.0) $e4

    map2 with function addition should return Some(3) on (Some(1), Some(2)) $e5
    map2 with function plusTen should return None on (None, Some(2)) $e6

    $sequenceForAllDefinedValue
    $sequenceForAtLeastOneNone
    $sequenceForEmptyList
  """




  // map
  private val plusTen: Int => Int = i => i + 10

  def e1 = None.map(plusTen) should equalTo(None)

  def e2 = Some(1).map(plusTen) should equalTo(Some(11))

  // flatMap
  private def divide(divide: Int, divisor: Int): Option[Int] = {
    try {
      Some(divide / divisor)
    } catch {
      case _: ArithmeticException => None
    }
  }

  def flatMapSome = Some(1).flatMap(i => divide(i, 1)) should equalTo(Some(1))

  def flatMapSomeDivideByZero = Some(1).flatMap(i => divide(i, 0)) should equalTo(None)

  def flatMapNone = None.map(i => divide(i, 1)) should equalTo(None)

  // getOrElse
  def getOrElseSome = Some(5).getOrElse(10) should equalTo(5)
  def getOrElseNone = None.getOrElse(10) should equalTo(10)

  // orElse
  def returnSome10: Option[Int] = {
    println("sleep10 called")
    Some(10)
  }

  def orElseSome = Some(1).orElse(returnSome10) should equalTo(Some(1))
  def orElseNone = None.orElse(returnSome10) should equalTo(Some(10))

  // filter
  val isEven: Int => Boolean = x => x % 2 == 0

  def filterEvenNumberFor2 = Some(2).filter(isEven) should equalTo(Some(2))

  def filterEvenNumberFor3 = Some(3).filter(isEven) should equalTo(None)

  def filterEvenNumberForNone = None.filter(isEven) should equalTo(None)

  // variance
  private val xs = Seq(1.0, 2.0, 4.0, 5.0)

  def e3 = variance(Seq.empty) should equalTo(None)

  def e4 = variance(xs) should equalTo(Some(2.5))


  // map 2
  val add: (Int, Int) => Int = (x, y) => x + y

  def e5 = map2(Some(1), Some(2))(add) should equalTo(Some(3))

  def e6 = map2(Some(1), None)(add) should equalTo(None)

  // sequence
  def sequenceForAllDefinedValue = sequence(List(Some(2), Some(3))) should equalTo(Some(List(2, 3)))

  def sequenceForAtLeastOneNone = sequence(List(Some(2), None)) should equalTo(None)

  def sequenceForEmptyList = sequence(List.empty[Option[Int]]) should equalTo(Some(Seq.empty))



}

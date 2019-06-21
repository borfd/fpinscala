import org.specs2.Specification

class Chapter3DropItSpec extends Specification { def is = s2"""
    As we saw building up data is easy
    and we end up sharing data between instances
    which lets us implement operations more efficiently.

    @TODO: Generalize #tail to the function drop,
    which removes the first n elements from a list.

    def drop[A](l: List[A], n: Int): List[A]

    @TODO: Implement #dropWhile,
    which removes elements from the List prefix
    as long as they match a predicate.

    def dropWhile[A](l: List[A], f: A => Boolean): List[A]

    drop(1, [3,4,5]) should return [4, 5] $e1
    drop(2, [3,4,5]) should return [5]    $e2
    drop(3, [3,4,5]) should return []     $e3

    dropWhile(x => x % 2 == 0, [1,2,3,4,5])
    should return []                      $e4
  """

  def threeFourFiveList: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def e1 =
    Chapter3Lists.drop(threeFourFiveList, 1) must equalTo(Cons(4, Cons(5, Nil)))

  def e2 =
    Chapter3Lists.drop(threeFourFiveList, 2) must equalTo(Cons(5, Nil))

  def e3 =
    Chapter3Lists.drop(threeFourFiveList, 3) must equalTo(Nil)

  def dropTheOdds: Int => Boolean = (x: Int) => x % 2 == 0

  def e4 =
    Chapter3Lists.dropWhile[Int](threeFourFiveList, dropTheOdds) must equalTo()

}

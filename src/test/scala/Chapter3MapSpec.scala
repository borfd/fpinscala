import org.specs2.Specification

class Chapter3MapSpec extends Specification { def is = s2"""
    Let's get some more practice generalizing functions
    and get some more familiarity with common patterns
    when processing lists.

    @TODO: write a function which transforms a list
    by adding +1 to each element such as:

    listPlusOne([3,4,5]) should equal to [4,5,6]         $e1

    @TODO: write a function which transforms a
    List[Double] to a List[String]                       $e2


    @TODO: write a function #map, which generalizes over
    the previous two functions                           $e3

    @TODO: write a function #filter,
    which removes elements from a list
    unless they satisfy a given predicate                $e4

    @TODO: write a function #flatMap that works
    like #map except that the supplied function
    shall return a list except a single result           $e5


    @TODO: write #filter using using #flatMap            $e6

    @TODO: write a function that accepts two lists
    and constructs a new list
    by adding corresponding elements                     $e7

    @TODO: generalize that function so it's
    not specific to integers or addition                 $e8

  """

  def threeFourFiveList: List[Int] = Cons(3, Cons(4, Cons(5, Nil)))

  def e1 =
    Chapter3Lists.listPlusOne(threeFourFiveList) should equalTo(
      Cons(4, Cons(5, Cons(6, Nil)))
    )

  def e2 =
    Chapter3Lists.listDoubleToString(
      Cons(3.toDouble, Cons(4.toDouble, Cons(5.toDouble, Nil)))
    ) should equalTo(
      Cons("3", Cons("4", Cons("5", Nil)))
    )

  def e3 =
    Chapter3Lists.map(threeFourFiveList)(_.toString) should equalTo(
      Cons("3", Cons("4", Cons("5", Nil)))
    )


  def e4 =
    Chapter3Lists.filter(threeFourFiveList)(_ % 2 == 0) should equalTo(
      Cons("3", Cons("5", Nil))
    )

  def e5 =
    Chapter3Lists.flatMap(threeFourFiveList)(x => Cons(1, Cons(x, Nil))) should equalTo(
      Cons(1, Cons(3, Cons(1, Cons(4, Cons(1, Cons(5, Nil))))))
    )

  def e6 =
    Chapter3Lists.filterWithFlatMap(threeFourFiveList)(_ % 2 == 0) should equalTo(
      Cons("3", Cons("5", Nil))
    )

  def e7 =
    Chapter3Lists.addTwoLists(threeFourFiveList, threeFourFiveList) should equalTo(
      Cons(6, Cons(8, Cons(10, Nil)))
    )

  def e8 =
    Chapter3Lists.zipWith(threeFourFiveList, threeFourFiveList)(_ + _) should equalTo(
      Cons(6, Cons(8, Cons(10, Nil)))
    )

}

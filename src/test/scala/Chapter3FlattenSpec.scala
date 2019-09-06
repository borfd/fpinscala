import org.specs2.mutable.Specification

class Chapter3FlattenSpec extends Specification {

  "when the list is empty" >> {
    "should return Nil" >> {
      Chapter3Lists.flatten(Nil) must equalTo(Nil)
    }
  }

  "when the list is not empty" >> {
    "should flatten lists" >> {
      val list1 = OurList(1, 2, 3)
      val list2 = OurList(4, 5 ,6)
      val list3 = OurList(7, 8, 9)

      val lists = OurList(list1, list2, list3)

      Chapter3Lists.flatten(lists) must equalTo(OurList(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
  }
}

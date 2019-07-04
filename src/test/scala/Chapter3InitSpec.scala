import org.specs2.mutable.Specification

class Chapter3InitSpec extends Specification {

  "when the list is empty" >> {
    "should return Nil" >> {
      Chapter3Lists.init(Nil) must equalTo(Nil)
    }
  }

  "when the list has one element" >> {
    "should return Nil" >> {
      Chapter3Lists.init(Cons("one", Nil)) must equalTo(Nil)
    }
  }

  "when the list two elements" >> {
    "should return Nil" >> {
      Chapter3Lists.init(Cons("one", Cons("two", Nil))) must equalTo(Cons("one", Nil))
    }
  }

  "when the list has more than two elements" >> {
    "should return Nil" >> {
      Chapter3Lists.init(Cons("one", Cons("two", Cons("three", Nil)))) must
          equalTo(Cons("one", Cons("two", Nil)))
    }
  }
}

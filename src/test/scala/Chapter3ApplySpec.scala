import org.specs2.mutable.Specification

class Chapter3ApplySpec extends Specification {

  "when no arguments provided" >> {
    "should return Nil" >> {
      OurList() must equalTo(Nil)
    }
  }

  "when the arguments provided" >> {
    "should construct a list" >> {
      OurList(1, 2, 3) must equalTo(Cons(1, Cons(2, Cons(3, Nil))))
    }
  }
}

import org.specs2.mutable.Specification

class Chapter3SubsequenceSpec extends Specification {

  "when the sub is empty" >> {
    "should return true" >> {
      Chapter3Lists.hasSubsequence(OurList(1, 2, 3), Nil) must equalTo(true)
    }
  }

  "when the sub equals to the list" >> {
    "should return true" >> {
      Chapter3Lists.hasSubsequence(OurList(1, 2, 3), OurList(1, 2, 3)) must equalTo(true)
    }
  }

  "when the list starts with the sub" >> {
    "should return true" >> {
      Chapter3Lists.hasSubsequence(OurList(1, 2, 3, 4), OurList(1, 2, 3)) must equalTo(true)
    }
  }

  "when the list contains the sub" >> {
    "should return true" >> {
      Chapter3Lists.hasSubsequence(OurList(0, 1, 2, 3, 4), OurList(1, 2, 3)) must equalTo(true)
    }
  }


  "when the list does not has the sub" >> {
    "should return false" >> {
      Chapter3Lists.hasSubsequence(OurList(0, 1, 2, 5, 3, 4), OurList(1, 2, 3)) must equalTo(false)
    }
  }
}

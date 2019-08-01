import Tree.{Branch, Leaf}
import org.specs2.mutable.Specification

class Chapter3TreesSpec extends Specification {

  val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  "size#" >> {
    "should return 1 for leaf" >> {
      Chapter3Trees.size(Leaf(123)) must equalTo(1)
    }

    "should return a total number of nodes" >> {
      Chapter3Trees.size(tree) must equalTo(5)
    }
  }

  "max#" >> {
    "should return the value for leaf" >> {
      Chapter3Trees.max(Leaf(123)) must equalTo(123)
    }

    "should return maximum leaf value" >> {
      Chapter3Trees.maxFold(tree) must equalTo(3)
    }
  }

  "depth#" >> {
    "should return 1 for leaf" >> {
      Chapter3Trees.depth(Leaf(123)) must equalTo(0)
    }

    "should return maximum depth" >> {
      Chapter3Trees.depth(tree) must equalTo(2)
    }
  }

  "map#" >> {
    "should transform value for a leaf" >> {
      Chapter3Trees.map(Leaf(123))(_.toString) must equalTo(Leaf("123"))
    }

    "should transform all values in the tree" >> {
      Chapter3Trees.map(tree)(_.toString) must equalTo(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
    }
  }


}

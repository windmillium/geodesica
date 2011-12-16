import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import world._

class WithIDSpec extends Spec with ShouldMatchers {
  object Foo extends WithIDObject[Foo]
  class Foo extends WithID[Foo] {
    def getObject = Foo
  }

  describe("id") {
    it("should start at 1") {
      val foo = new Foo
      foo.id should equal(1)
    }

    it("should increment") {
      val foo = new Foo
      val id = foo.id
      val foo2 = new Foo
      foo.id should equal(foo2.id-1)
    }
  }

  describe("registry") {
    it("should contain new objects") {
      val foo = new Foo
      Foo.all.contains(foo) should equal(true)
    }
  }
}


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable.ListBuffer

import world._

class PlantSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
  describe("Class") {
    it("should have an age") {
      val block = new Block(0,0,0)
      val species = new PlantSpecies("test")
      val plant = species.create(block)
      plant.age should equal(0)
    }
  }
}

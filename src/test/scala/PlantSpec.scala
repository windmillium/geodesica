import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable.ListBuffer

import world._

class PlantSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
  describe("Object") {
    it("should have a list of all plants") {
      val plants: ListBuffer[Plant] = Plant.all
    }

    describe("nextId") {
      it("should return an integer") {
        val id: Int = Plant.nextId
      }

      it("should increment the id") {
        val id = Plant.id
        Plant.nextId should equal(id+1)
      }
    }
  }

  describe("Class") {
    it("should have an id") {
      val plant = new Plant
      val id: Int = plant.id
    }

    it("should have an age") {
      val plant = new Plant
      plant.age should equal(0)
    }

    it("should add to the global plants list") {
      val plant = new Plant
      Plant.all.last should equal(plant)
    }
  }
}

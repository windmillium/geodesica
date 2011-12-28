import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable.ListBuffer

import net.geodesica._

class PlantSpec extends Spec {

  def fixture =
    new {
      val block = new Block(new Coord(0,0,0))
      val cot = new ObjectTemplate("Stick")
      val dot = new ObjectTemplate("Wood")
      val species = new PlantSpecies("test", cot, dot)
      val plant = species.create(block)
    }

  describe("Class") {
    it("should have an age") {
      val f = fixture
      expect(0){
        f.plant.age
      }
    }
  }

  describe("death") {
    it("should create a deathTemplate object in the block") {
      val f = fixture
      f.species.create(f.block).die
      expect(f.block.objects.filter(o=>o.template == f.dot).size) {
        1
      }
    }

    it("should remove itself from the block"){
      val f = fixture
      f.block.plant = f.species.create(f.block)
      f.block.plant.die
      expect(null) {
        f.block.plant
      }
    }
  }

  describe("update") {
    it("should increment the plant age"){
      val f = fixture
      f.plant.update
      expect(1){
        f.plant.age
      }
    }

    it("should add to the crop every 1000 updates"){
      val f = fixture
      f.plant.crop = 0
      for(i <- 0 until 2999){
        f.plant.update
      }
      expect(2){
        f.plant.crop
      }
    }
  }

  describe("object update"){
    it("should update all the plants"){
      val f = fixture
      val plant1 = f.species.create(f.block)
      val plant2 = f.species.create(f.block)
      val age = plant1.age + plant2.age
      Plant.update
      expect(age+2){
        plant1.age+plant2.age
      }
    }
  }
}

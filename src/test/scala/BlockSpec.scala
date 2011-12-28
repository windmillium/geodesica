import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

class BlockSpec extends Spec {
  describe("heuristic") {
    it("should measure the distance from the target block"){
      val block1 = new Block(new Coord(0,0,0))
      val block2 = new Block(new Coord(3,20,0))
      expect(block1.distanceFrom(block2).toInt){
        block1.heuristic(block2)
      }
    }
  }

  describe("adjacent"){
    it("should return the cardinal direction blocks which are open"){
      val world = new World(3)
      val block1 = world.blockAt(new Coord(1,0,0)).get
      val block2 = world.blockAt(new Coord(1,2,0)).get
      block1.health = 0
      block2.health = 0
      expect(List(block1,block2)){
        world.blockAt(new Coord(1,1,0)).get.adjacent
      }
    }
  }

  describe("canAccept"){
    it("should return None if health > 0"){
      val block = new Block(new Coord(0,0,0))
      block.health = 1
      expect(None){
        block.canAccept
      }
    }
    it("should return Some(block) if health = 0"){
      val block = new Block(new Coord(0,0,0))
      block.health = 0
      expect(Some(block)){
        block.canAccept
      }
    }
  }
}

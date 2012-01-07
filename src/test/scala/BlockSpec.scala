import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

class CoordSpec extends Spec {
  describe("distanceFrom") {
    it("should return the distance between two coords") {
      expect(true){
        math.abs(new Coord(2,0,1).distanceFrom(new Coord(3,20,0))-20.04) < 0.01
      }
    }
  }

  describe("addition") {
    it("should add the coords and return a new coord") {
      expect(new Coord(2,4,6)){
        new Coord(1,2,3) + (1,2,3)
      }
    }
  }
}

class BlockSpec extends Spec {
  describe("heuristic") {
    it("should measure the distance from the target block"){
      val blockMap = new BlockMap
      val block1 = new Block(blockMap,new Coord(0,0,0))
      val block2 = new Block(blockMap,new Coord(3,20,0))
      expect(block1.distanceFrom(block2).toInt){
        block1.heuristic(block2)
      }
    }
  }

  describe("adjacent"){
    it("should return the cardinal direction blocks which are open"){
      val blockMap = new BlockMap
      new Block(blockMap, new Coord(2,1,0))
      new Block(blockMap, new Coord(0,1,0))
      new Block(blockMap, new Coord(1,1,0))
      new Block(blockMap, new Coord(2,0,0))
      new Block(blockMap, new Coord(0,0,0))
      new Block(blockMap, new Coord(1,0,0))
      new Block(blockMap, new Coord(2,2,0))
      new Block(blockMap, new Coord(0,2,0))
      new Block(blockMap, new Coord(1,2,0))
      val block1 = blockMap.blockAt(new Coord(1,0,0)).get
      val block2 = blockMap.blockAt(new Coord(1,2,0)).get
      block1.health = 0
      block2.health = 0
      expect(List(block2,block1)){
        blockMap.blockAt(new Coord(1,1,0)).get.adjacent
      }
    }
  }

  describe("canAccept"){
    it("should return false if health > 0"){
      val blockMap = new BlockMap
      val block = new Block(blockMap,new Coord(0,0,0))
      block.health = 1
      expect(false){
        block.canAccept
      }
    }
    it("should return true if health = 0"){
      val blockMap = new BlockMap
      val block = new Block(blockMap,new Coord(0,0,0))
      block.health = 0
      expect(true){
        block.canAccept
      }
    }
  }
}

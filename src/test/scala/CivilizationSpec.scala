import org.scalatest.Spec

import net.geodesica._

class CivilizationSpec extends Spec {
  class TestZone extends Zone {
    def requirements = new collection.mutable.HashSet[(net.geodesica.Block, Option[net.geodesica.ObjectTemplate])]
  }

  describe("homes"){
    it("should return only home zones"){
      val civ = new Civilization("test")
      civ.zones += new Home
      civ.zones += new Home
      civ.zones += new TestZone
      expect(2){
        civ.homes.size
      }
    }
  }

  describe("stockpiles"){
    it("should return only stockpile zones"){
      val civ = new Civilization("blah")
      civ.zones += new TestZone
      civ.zones += new Stockpile
      civ.zones += new Stockpile

      expect(2){
        civ.stockpiles.size
      }
    }
  }
  describe("stockpileCapacity") {
    it("should return zero if no stockpiles"){
      val civ = new Civilization("testing")
      expect(0){
        civ.stockpileCapacity
      }
    }

    it("should return the sum of the stockpiles capacities"){
      val civ = new Civilization("testing")
      val sp  =  new Stockpile
      val blockMap = new BlockMap
      val block1 = new Block(blockMap,new Coord(0,0,0))
      val block2 = new Block(blockMap,new Coord(3,20,0))
      val block3 = new Block(blockMap,new Coord(3,20,0))

      val ot = new ObjectTemplate("container")
      block1.installedObject = new ContainerObject(ot, 24)
      block2.installedObject = new ContainerObject(ot, 12)
      sp.blocks ++= List(block1,block2,block3)
      civ.zones += sp

      expect(36){
        civ.stockpileCapacity
      }
    }
  }
}


class StockpileSpec extends Spec {
  describe("capacity"){
    it("should return zero if no blocks"){
      val sp = new Stockpile

      expect(0){
        sp.capacity
      }
    }
    it("should return the sum of the blocks capacity"){
      val sp = new Stockpile
      val ot = new ObjectTemplate("blah")
      val container = new ContainerObject(ot,37)
      val container2 = new ContainerObject(ot,3)
      val blockMap = new BlockMap
      val block1 = new Block(blockMap,new Coord(0,0,0))
      val block2 = new Block(blockMap,new Coord(0,0,0))
      val block3 = new Block(blockMap,new Coord(0,0,0))
      block1.installedObject = container
      block2.installedObject = container2
      sp.blocks += block1
      sp.blocks += block2
      sp.blocks += block3

      expect(40){
        sp.capacity
      }
    }
  }
}

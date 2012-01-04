import org.scalatest.Spec

import net.geodesica._

class ProfessionSpec extends Spec {
  describe("zoning homes") {
    it("should return none if homes is >= mobiles in civilization") {
      val civ = new Civilization("")
      val mob = new Mobile
      val blockMap = new BlockMap
      val block = new Block(blockMap,new Coord(0,0,0))
      civ.home = block
      civ.zones += new Home
      mob.assignCivilization(civ)
      expect(None){
        Planning.createHomeJob(civ)
      }
    }
    it("should create a new ZoneHomeJob if homes is less than mobiles in civilization") {
      val civ = new Civilization("")
      val mob = new Mobile
      val blockMap = new BlockMap
      val block = new Block(blockMap,new Coord(0,0,0))
      civ.home = block
      mob.assignCivilization(civ)
      assert(Planning.createHomeJob(civ).get.isInstanceOf[ZoneHomeJob])
    }
  }
}

class PlanningSpec extends Spec {
}

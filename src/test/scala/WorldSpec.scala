// import org.scalatest.Spec
// import org.scalatest.matchers.ShouldMatchers
// import org.scalatest.BeforeAndAfterAll

// import world._

// class WorldSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
//   var world : World = _

//   override def beforeAll = {
//     world = new World(6)
//   }

//   describe("indexFor") {
//     it("should return an index for a coordinate") {
//       world.indexFor(1,2,0) should equal(25)
//       world.indexFor(2,2,0) should equal(26)
//     }
//   }

//   describe("blockAt") {
//     it("should return the block given a certain index") {
//       world.blockAt(1,2,0) should equal(world.map(25))
//     }
//   }

//   describe("createWorld") {
//     it("should set the health to 0 for everything half way from left") {
//       world.blockAt(0,0,0).health should equal(0)
//       world.blockAt(10,0,0).health should equal(100)
//     }

//     it("should populate the entire map") {
//       world.map.size should equal(72)
//     }
//   }
// }

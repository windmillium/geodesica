package net.geodesica

class SpaceFinder(blocks:Set[Block], desiredSize:Int) {
  val location:Option[Block] = blocks.find(b => b.nearbyBlocks(desiredSize).forall({case(c,nb) => nb.canBuild}))
}

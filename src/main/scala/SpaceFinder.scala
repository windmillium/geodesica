package net.geodesica

class SpaceFinder(blocks:Set[Block], desiredSize:Int) {
  val location:Option[Block] = blocks.find(b => b.nearbyBlocks(desiredSize).flatten.forall(nb => nb.canBuild))
}

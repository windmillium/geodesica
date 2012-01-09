package net.geodesica

class SpaceFinder(blocks:Set[Block], desiredSize:Int) {
  val location:Option[Block] = blocks.find(b => b.nearbyBlocks(desiredSize+2).flatten.forall(nb => nb.canBuild)).flatMap(b =>
      b.blockAt(b.coord+(1,1,0)))
}

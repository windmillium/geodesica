package net.geodesica

import collection.mutable.ListBuffer
import collection.mutable.HashSet

class Civilization(val name:String) {
  val queue = new JobQueue(name)
  val recipes = new ListBuffer[Recipe]
  var home: Block = _
  val zones = new ListBuffer[Zone]
  val objects = new HashSet[Object]
  def freeObjects = {
    objects.filter(o=>o.free)
  }

  def blocks = home.nearbyBlocks(15)

  def stockpiles = zones.collect({ case z:Stockpile => z})
  def workshops = zones.collect({ case z:Workshop => z})
  def halls = zones.collect({case z:Hall => z})
  def stockpileCapacity:Int = {
    stockpiles.foldLeft(0)(_ + _.capacity)
  }
}

trait Zone

class Stockpile extends Zone {
  import ObjectTemplate._
  val blocks = new ListBuffer[Block]
  def capacity = blocks.foldLeft(0)(_ + _.objectCapacity)

  def finished = !blocks.exists(b => b.installedObject == null)

  def objects = blocks.map(b => objectTemplate("Pallet")).flatten

  def requirements = blocks.map(b => (b,objectTemplate("Pallet")))
}

class Workshop extends Zone {
  import ObjectTemplate._

  val blocks = new BlockSet

  def finished = {
    blocks.enclosed &&
    blocks.contains(1, objectTemplate("Table").get) &&
    blocks.contains(1, objectTemplate("Bench").get)
  }

  def requirements = {
    blocks.enclosingBlocks.map(b => (b,objectTemplate("Fence"))) ++
    (blocks -- blocks.enclosingBlocks).slice(0,1).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(1,2).map(b => (b,objectTemplate("Bench")))

  }
}

class Hall extends Zone {
  import ObjectTemplate._

  val blocks = new BlockSet

  def finished = {
    blocks.enclosed
  }

  def requirements = blocks.enclosingBlocks.map(b => (b,objectTemplate("Fence")))
}

class BlockSet extends HashSet[Block] {
  def enclosed = {
    enclosingBlocks.filter(b => b.installedObject != null).size == 0
  }

  def enclosingBlocks = {
    filter(b =>
      b.coord.x == max.coord.x ||
      b.coord.x == min.coord.x ||
      b.coord.y == max.coord.y ||
      b.coord.y == min.coord.y
    )
  }

  def contains(amount:Int, ot:ObjectTemplate) = {
    filter(b => b.installedObject != null && b.installedObject.template == ot).size >= amount
  }
}

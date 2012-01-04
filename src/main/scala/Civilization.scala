package net.geodesica

import collection.mutable.ListBuffer
import collection.mutable.HashSet

class Civilization(val name:String) {
  val queue = new JobQueue(name)
  val recipes = new ListBuffer[Recipe]
  var home: Block = _
  val zones = new ListBuffer[Zone]
  val objects = new HashSet[Object]
  val mobiles = new HashSet[Mobile]

  def freeObjects = {
    objects.filter(o=>o.free)
  }

  def blocks = home.nearbyBlocks(20)

  def stockpiles = zones.collect({ case z:Stockpile => z})
  def workshops = zones.collect({ case z:Workshop => z})
  def halls = zones.collect({case z:Hall => z})
  def homes = zones.collect({case z:Home => z})
  def stockpileCapacity:Int = {
    stockpiles.foldLeft(0)(_ + _.capacity)
  }
}

trait Zone {
  val blocks = new BlockSet
}


trait Construction

object House {
  import ObjectTemplate._
  def requirements(blocks:BlockSet) = {
    val doorBlock = blocks.min.blockAt(blocks.min.coord+(1,0,0)).get
    (blocks.enclosingBlocks-=doorBlock).map(b => (b,objectTemplate("Fence"))) ++
    (blocks -- blocks.enclosingBlocks).slice(0,1).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(1,2).map(b => (b,objectTemplate("Bed"))) +=
    (doorBlock -> objectTemplate("Door"))
  }
}

object Shop {
  import ObjectTemplate._
  def requirements(blocks:BlockSet) = {
    val doorBlock = blocks.min.blockAt(blocks.min.coord+(1,0,0)).get
    (blocks.enclosingBlocks-=doorBlock).map(b => (b,objectTemplate("Fence"))) ++
    (blocks -- blocks.enclosingBlocks).slice(0,1).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(1,2).map(b => (b,objectTemplate("Bench"))) +=
    (doorBlock -> objectTemplate("Door"))
  }
}

object Lodge {
  import ObjectTemplate._

  def requirements(blocks:BlockSet) = {
    val doorBlock = blocks.min.blockAt(blocks.min.coord+(1,0,0)).get
    (blocks.enclosingBlocks-=doorBlock).map(b => (b,objectTemplate("Fence"))) ++
    (blocks -- blocks.enclosingBlocks).slice(0,1).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(1,2).map(b => (b,objectTemplate("Bench"))) ++
    (blocks -- blocks.enclosingBlocks).slice(2,3).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(3,4).map(b => (b,objectTemplate("Bench"))) ++
    (blocks -- blocks.enclosingBlocks).slice(4,5).map(b => (b,objectTemplate("Table"))) ++
    (blocks -- blocks.enclosingBlocks).slice(5,6).map(b => (b,objectTemplate("Bench"))) +=
    (doorBlock -> objectTemplate("Door"))
  }
}

object FlatStockpile {
  import ObjectTemplate._
  def requirements(blocks:BlockSet) = blocks.map(b => (b,objectTemplate("Pallet")))
}

class Stockpile extends Zone {
  def capacity = blocks.foldLeft(0)(_ + _.objectCapacity)

  def requirements = FlatStockpile.requirements(blocks)
}
class Workshop extends Zone {
  def requirements = Shop.requirements(blocks)
}

class Hall extends Zone {
  def requirements = Lodge.requirements(blocks)
}

class Home extends Zone {
  def requirements = House.requirements(blocks)
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

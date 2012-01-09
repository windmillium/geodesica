package net.geodesica

import collection.mutable.ListBuffer
import collection.mutable.HashSet

class Civilization(val name:String) {
  val queue = new JobQueue
  val recipes = new ListBuffer[Recipe]
  var home: Block = _
  val zones = new ListBuffer[Zone]
  val objects = new HashSet[Object]
  val mobiles = new HashSet[Mobile]

  def freeObjects = {
    objects.filter(o=>o.free)
  }

  def blocks = home.nearbyBlocks(50)

  def stockpiles = zones.collect({ case z:Stockpile => z})
  def workshops = zones.collect({ case z:Workshop => z})
  def halls = zones.collect({case z:Hall => z})
  def homes = zones.collect({case z:Home => z})
  def farms = zones.collect({case z:Farm => z})
  def barracks = zones.collect({case z:Barracks => z})
  def stockpileCapacity:Int = {
    stockpiles.foldLeft(0)(_ + _.capacity)
  }

  def nextZone:Option[(Symbol, Int)] = {
    if(stockpiles.size == 0) {
      Some('Stockpile, 3)
    } else if(workshops.size == 0) {
      Some('Workshop, 5)
    } else if(halls.size == 0) {
      Some('Hall, 6)
    } else if(barracks.size == 0) {
      Some('Barracks, 6)
    } else if(mobiles.size > homes.size) {
      Some('Home, 5)
    } else if(farms.size == 0) {
      Some('Farm, 7)
    } else {
      None
    }
  }
}

trait Zone {
  val blocks = new BlockSet
  def requirements:HashSet[(net.geodesica.Block, Option[net.geodesica.ObjectTemplate])]
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

object Field {
  import ObjectTemplate._

  def requirements(blocks:BlockSet) = {
    val doorBlock = blocks.min.blockAt(blocks.min.coord+(1,0,0)).get
    (blocks.enclosingBlocks-=doorBlock).map(b => (b,objectTemplate("Fence")))
  }
}

object Armory {
  import ObjectTemplate._
  def requirements(blocks:BlockSet) = {
    val doorBlock = blocks.min.blockAt(blocks.min.coord+(1,0,0)).get
    (blocks.enclosingBlocks-=doorBlock).map(b => (b,objectTemplate("Fence"))) ++
    (blocks -- blocks.enclosingBlocks).slice(0,1).map(b => (b,objectTemplate("Armor Stand"))) ++
    (blocks -- blocks.enclosingBlocks).slice(1,2).map(b => (b,objectTemplate("Weapon Stand"))) +=
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

class Barracks extends Zone {
  def requirements = Armory.requirements(blocks)
}

class Home extends Zone {
  def requirements = House.requirements(blocks)
}

class Farm extends Zone {
  def requirements = Field.requirements(blocks)

  def finishedPlants = blocks.filter(b => b.plant != null && b.plant.crop > 0).map(b => b.plant)
  def freeBlock = (blocks -- blocks.enclosingBlocks).find(b => b.plant == null)
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

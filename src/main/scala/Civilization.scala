package net.geodesica

import collection.mutable.ListBuffer

class Civilization(val name:String) {
  val queue = new JobQueue(name)
  val recipes = new ListBuffer[Recipe]
  var home: Block = _
  val zones = new ListBuffer[Zone]
  val objects = new ListBuffer[Object]

  def blocks = home.nearbyBlocks(10)

  def stockpiles = zones.collect({ case z:Stockpile => z})
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

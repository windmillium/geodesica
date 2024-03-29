package net.geodesica
import scala.collection.mutable.ListBuffer

case class Coord(x:Int,y:Int,z:Int) {
  def atXPlus(amount:Int) = new Coord(x+amount,y,z)
  def atYPlus(amount:Int) = new Coord(x,y+amount,z)
  def atZPlus(amount:Int) = new Coord(x,y,z+amount)

  def +(coord:(Int,Int,Int)) = {new Coord(x+coord._1, y+coord._2, z+coord._3)}

  def distanceFrom(target: Coord): Double = {
    import math._
    sqrt(pow(x - target.x, 2) + pow(y - target.y,2) + pow(z - target.z,2))
  }
}

class Block(val blockMap:BlockMap = new BlockMap,
            val coord:Coord = new Coord(0,0,0),
            var health: Int = 100,
            val objectTemplate:ObjectTemplate = new ObjectTemplate("Generic Item"))
extends Attackable
  with Container
  with Ordered[Block]
  with Searchable[Block] {

  def compare(that:Block):Int = {
    coord.x + coord.y + coord.z - that.coord.x - that.coord.y - that.coord.z
  }

  var plant: Plant = _
  var mobiles = new ListBuffer[Mobile]
  var selected = false
  var installedObject:Object = _
  var zone:Zone = _
  var block = this
  var civilization:Civilization = _
  import scala.collection.mutable.Set

  val classes = new ListBuffer[String]

  blockMap.blocks += ((coord.x,coord.y,coord.z) -> this)

  def blockAt(coord:Coord) = blockMap.blockAt(coord)
  def north = {
    val newCoord = coord + (0,1,0)
    blockAt(newCoord)
  }
  def east = {
    val newCoord = coord + (1,0,0)
    blockAt(newCoord)
  }
  def south = {
    val newCoord = coord + (0,-1,0)
    blockAt(newCoord)
  }
  def west = {
    val newCoord = coord + (-1,0,0)
    blockAt(newCoord)
  }

  def canBuild = {
    installedObject == null &&
    zone == null &&
    plant == null &&
    canAccept
  }

  def layers = {
    var tclasses = new ListBuffer[String]

    if(health == 0)
      tclasses += "grass"
    else
      tclasses += "stone"

    if(installedObject != null)
      tclasses += installedObject.kind
    if(selected)
      tclasses += "selected"

    tclasses ++= objects.map(o => o.kind)

    if(plant != null)
      tclasses += plant.species.name

    if(zone != null)
      tclasses += "zone"


    tclasses

  }

  def plantWithCrop = {
    plant != null && plant.crop > 0
  }

  def freeObjects = {
    objects.filter(o => o.free)
  }
  def objectCapacity = {
    installedObject match {
      case null => 0
      case _ => installedObject.capacity
    }
  }

  def cost:Int = 1

  def heuristic(target:Block):Int = {
    implicit def double2int(d:Double):Int = d.toInt
    distanceFrom(target)
  }

  def adjacent:List[Block] = {
    List(north,south,east,west).flatten.filter(b => b.canAccept)
  }

  def nearbyBlocks(distance:Int):List[Option[Block]] = {
    blockMap.blocksAt(coord.x.until(coord.x+distance).map(x => coord.y.until(coord.y+distance).map(y => new Coord(x,y,0))).flatten.toList)
  }

  def distanceFrom(target:Block) = {
    coord.distanceFrom(target.coord)
  }

  def die = {
    objectTemplate.create.moveTo(this)
  }

  def canAccept = {
    health == 0 && (installedObject == null || installedObject.template.name != "Fence")
  }

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> coord.x) ~
      ("y" -> coord.y) ~
      ("z" -> coord.z) ~
      ("health" -> health) ~
      ("layers" -> layers)
    compact(render(json))
  }

  override def toString = {
    "%s,%s,%s".format(coord.x,coord.y,coord.z) +
    " Plant" + plant
  }
}



package net.geodesica
import scala.collection.mutable.ListBuffer

case class Coord(x:Int,y:Int,z:Int) {
  def atXPlus(amount:Int) = new Coord(x+amount,y,z)
  def atYPlus(amount:Int) = new Coord(x,y+amount,z)
  def atZPlus(amount:Int) = new Coord(x,y,z+amount)
}

trait BlockWorld {
  val world:World
  val coord:Coord

  def north = { world.blockAt(coord.atYPlus(1).x,coord.atYPlus(1).y,coord.atYPlus(1).z) }
  def east  = { world.blockAt(coord.atXPlus(1).x,coord.atXPlus(1).y,coord.atXPlus(1).z) }
  def south = { world.blockAt(coord.atYPlus(-1).x,coord.atYPlus(-1).y,coord.atYPlus(-1).z) }
  def west  = { world.blockAt(coord.atXPlus(-1).x,coord.atXPlus(-1).y,coord.atXPlus(-1).z) }
}

class Block(val world:World, val x: Int, val y: Int, val z: Int, var health: Int = 100, val objectTemplate:ObjectTemplate = new ObjectTemplate("Generic Item")) extends BlockWorld with Attackable with Searchable[Block] {
  var plant: Plant = _
  var mobiles = new ListBuffer[Mobile]
  var selected = false
  var objects = new ListBuffer[Object]
  val coord = new Coord(x,y,z)
  import scala.collection.mutable.HashSet

  val classes = new HashSet[String]

  def allClasses = {
    var tclasses = new HashSet[String]
    if(objects.size > 0)
      tclasses += "object"
    if(plant != null)
      tclasses += "plant"
    if(selected)
      tclasses += "selected"
    classes.union(tclasses)
  }

  def cost = 1

  def heuristic(target:Block):Int = {
    implicit def double2int(d:Double):Int = d.toInt
    distanceFrom(target)
  }

  def adjacent:List[Block] = {
    List(north,south,east,west).flatten.filter(b => b.canAccept != None)
  }

  def distanceFrom(nblock: Block): Double = {
    import math._
    sqrt(pow(x - nblock.x, 2) + pow(y - nblock.y,2))
  }

  def die = {
    val obj = objectTemplate.create
    objects += obj
    obj.block = this
  }

  def canAccept = {
    health match {
      case 0 => Some(this)
      case _ => None
    }
  }

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> x) ~
      ("y" -> y) ~
      ("z" -> z) ~
      ("health" -> health) ~
      ("classes" -> allClasses.mkString(" "))
    compact(render(json))
  }

  override def toString = {
    "%s,%s,%s".format(x,y,z)
  }
}



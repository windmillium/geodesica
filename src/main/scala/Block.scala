package world
import scala.collection.mutable.ListBuffer

import cc.spray.json._
import DefaultJsonProtocol._

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit object BlockFormat extends JsonFormat[Block] {
    def write(b: Block) = JsObject(List(
        JsField("x", b.x),
        JsField("y", b.y),
        JsField("z", b.z),
        JsField("health", b.health),
        JsField("plants", b.plants),
        JsField("mobiles", b.mobiles.size),
        JsField("selected", b.selected),
        JsField("objects", b.objects.size)
    ))
    def read(value: JsValue) = {
      value match {
      case JsObject(List(
        JsField("x", JsNumber(x)),
        JsField("y", JsNumber(y)),
        JsField("z", JsNumber(z)),
        JsField("health", JsNumber(health)),
        JsField("plants", JsString(plants)),
        JsField("mobiles", JsNumber(mobiles)),
        JsField("selected", JsBoolean(select)),
        JsField("objects", JsNumber(objects)))
      ) => {
        val pblock = WorldController.world.blockAt(x.toInt,y.toInt,z.toInt)
        pblock match {
          case None => throw new DeserializationException("no such block")
          case Some(block) => {
            block.selected = select
            block
          }
        }
      }
      case _ => throw new DeserializationException("Block expected")
    }

    }
  }
}

import MyJsonProtocol._

class Block(val x: Int, val y: Int, val z: Int, var health: Int = 100, val objectTemplate:ObjectTemplate = new ObjectTemplate("Generic Item")) extends Attackable with Searchable[Block] {
  var plant: Plant = _
  var mobiles = new ListBuffer[Mobile]
  var selected = false
  var objects = new ListBuffer[Object]
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
    List(north,south,east,west).flatten.filter(b => b.health == 0)
  }

  def distanceFrom(nblock: Block): Double = {
    import math._
    sqrt(pow(x - nblock.x, 2) + pow(y - nblock.y,2))
  }


  def plants = {
    val pl = Option(plant)
    pl match {
      case Some(x) => "1"
      case None => "0"
    }
  }

  def hasSpace = {
  }

  def die = {
    val obj = objectTemplate.create
    objects += obj
    obj.block = this
  }

  def north = {
    WorldController.world.blockAt(x,y+1,z)
  }
  def east = {
    WorldController.world.blockAt(x+1,y,z)
  }
  def south = {
    WorldController.world.blockAt(x,y-1,z)
  }
  def west = {
    WorldController.world.blockAt(x-1,y,z)
  }

  def canAccept(mobile: Mobile) = {
    health match {
      case 0 => Some(this)
      case _ => None
    }
  }

  def json = {
    this.toJson
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



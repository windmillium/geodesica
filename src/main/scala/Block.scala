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

class Block( val x: Int, val y: Int, val z: Int, var health: Int = 100 ) extends Attackable {
  var plant: Plant = _
  var mobiles = new ListBuffer[Mobile]
  var selected = false
  var objects = new ListBuffer[Object]

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
    objects += new Object(this)
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
    import MyJsonProtocol._
    this.toJson
  }
}



package world

import scala.collection.mutable.ListBuffer

import cc.spray.json._
import DefaultJsonProtocol._

object BuildingJsonProtocol extends DefaultJsonProtocol {
  implicit object BuildingFormat extends JsonFormat[Building] {
    def write(b: Building) = JsObject(List(
        JsField("name", b.name)
    ))
    def read(value: JsValue) = {
      new Building("test")
    }
  }
}

class Building(val name: String) {
  Building.templates += this
}

object Building {
  val templates = new ListBuffer[Building]

  new Building("storage")

  def toJson = {
    import BuildingJsonProtocol._
    var js: String = "["
    js += templates.map( m => m.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    js
  }
}


package world

import scala.collection.mutable.ListBuffer

import cc.spray.json._
import DefaultJsonProtocol._

object BuildingJsonProtocol extends DefaultJsonProtocol {
  implicit object BuildingTemplateFormat extends JsonFormat[BuildingTemplate] {
    def write(b: BuildingTemplate) = JsObject(List(
        JsField("name", b.name)
    ))
    def read(value: JsValue) = {
      new BuildingTemplate("test")
    }
  }
}

class BuildingTemplate(val name: String) {
  BuildingTemplate.templates += this
}

object BuildingTemplate {
  val templates = new ListBuffer[BuildingTemplate]

  new BuildingTemplate("storage")

  def toJson = {
    import BuildingJsonProtocol._
    var js: String = "["
    js += templates.map( m => m.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    js
  }
}


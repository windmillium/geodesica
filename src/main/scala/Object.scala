package net.geodesica
import scala.collection.mutable.ListBuffer

object Object extends WithIDObject[Object] {
  def availableObjects = {
    all.filter(o => o.container.isInstanceOf[Block])
  }
}

class ContainerObject(template:ObjectTemplate, override val capacity:Int) extends Object(template)

trait Container {
  def block:Block
  val objects = new ListBuffer[Object]
  var civilization:Civilization
}

class Object(val template:ObjectTemplate, var health:Int = 100)
  extends WithID[Object]
  with Container
  with Attackable {
  var container:Container = _

  def block = container.block
  var civilization:Civilization = _

  def name = template.name
  def kind = template.kind
  def capacity = 0

  var installed:Block = _

  def getObject = Object

  def die = {
    
  }

  def destroy = {
    if(container != null)
      container.objects -= this

    if(civilization != null)
      civilization.objects -= this
  }

  def free = {
    container.isInstanceOf[Block] && installed == null
  }

  def moveTo(target:Container) = {
    if(container != null)
      container.objects -= this
    container = target
    container.objects += this

    if(target.civilization != null) {
      civilization = target.civilization
      civilization.objects += this
    }
  }

  override def toString = {
    var str = name+":"+id
    str
  }

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("name" -> name)
    compact(render(json))
  }
}

object ObjectTemplate extends WithIDObject[ObjectTemplate] {
  def objectTemplate(name:String) = all.find(ot => ot.name == name)
}

class ObjectTemplate(val name:String, val kind:String = "Object", val requirements:Requirement = new Requirement) extends WithID[ObjectTemplate] {
  def getObject = ObjectTemplate

  def create = {
    new Object(this)
  }

  override def toString = {
    name
  }
}

package net.geodesica

object Object extends WithIDObject[Object] {
  def availableObjects = {
    all.filter(_.block != null)
  }
}

class Object(val template:ObjectTemplate, var health:Int = 100) extends WithID[Object] with Attackable {
  var block:Block = _

  def name = template.name
  def kind = template.kind
  def capacity = 0

  def getObject = Object

  def die = {
    
  }

  def moveTo(nblock:Block) = {
    block = nblock
    block.objects += this
  }

  override def toString = {
    var str = name+":"+id
    if(block != null)
      str += ",block:"+block
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

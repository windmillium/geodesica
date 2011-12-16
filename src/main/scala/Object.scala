package world

object Object extends WithIDObject[Object] {
  def availableObjects = {
    all.filter(_.block != null)
  }
}
class Object(var block:Block) extends WithID[Object] {
  def getObject = Object
}


package world

trait WithID[A] {
  self: A =>
  var id: Int = getObject.nextId
  getObject.all += this

  def getObject:WithIDObject[A]
}

trait WithIDObject[A] {
  import collection.mutable.ListBuffer
  var id: Int = 0
  val all = new ListBuffer[A]

  def nextId = {
    id += 1
    id
  }
}

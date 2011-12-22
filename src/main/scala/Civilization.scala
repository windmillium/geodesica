package world

import collection.mutable.ListBuffer

class Civilization(val name:String) {
  val queue = new JobQueue(name)
  val recipes = new ListBuffer[Recipe]
}

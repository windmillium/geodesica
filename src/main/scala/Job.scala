package net.geodesica

class Job(val queue: JobQueue, val profession:String = "General", val requirements:Requirement = new Requirement) {
  var block: Block = _
  var owner: Option[Mobile] = None
  def getObject = queue
  def work = ()
  def finalTask:Option[Task] = None
  def finished = false

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.coord.x) ~
      ("y" -> block.coord.y) ~
      ("z" -> block.coord.z) ~
      ("type" -> profession)
    compact(render(json))
  }
}

class JobQueue(name:String) extends WithIDObject[Job] {
  import collection.mutable.ListBuffer

  def openJobs:ListBuffer[Job] = {
    all.filter(_.owner == None)
  }
  def findJob(professions:ListBuffer[String]):Option[Job] = {
    openJobs.filter(j => professions.contains(j.profession)).headOption
  }

  def toJson = {
    var js: String = "["
    js += all.filter(j => j.block != null).map(j => j.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    js
  }
}

class HarvestJob(queue:JobQueue) extends Job(queue, "Gardening", new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant.crop == 0
  }
  override def finalTask = {
    Some(new HarvestTask(block.plant))
  }
}

class ClearJob(queue:JobQueue) extends Job(queue, "WoodWorking", new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null
  }

  override def finalTask = {
    Some(new ClearTask(block.plant))
  }
}

class CraftJob(queue:JobQueue, recipe:Recipe) extends Job(queue, "Crafting", recipe.requirements) with WithID[Job] {
  var newObject:Object = _
  override def finished = {
    newObject != null && newObject.health == 100
  }
  override def finalTask = {
    newObject = owner.get.craftInitial(recipe)
    Some(new CraftTask(newObject))
  }

  override def toString = {
    "Craft: " + recipe
  }
}

class DigJob(queue:JobQueue) extends Job(queue, "Mining", new Requirement(2)) with WithID[Job] {
  override def finished:Boolean = {
    block.health == 0
  }
  override def finalTask = {
    Some(new AttackTask(block))
  }
}

class BuildJob(queue:JobQueue) extends Job(queue, "Building", new Requirement(2)) with WithID[Job] {
  override def finished = {
    block.health == 100
  }
  override def finalTask = {
    Some(new BuildTask(block))
  }
}

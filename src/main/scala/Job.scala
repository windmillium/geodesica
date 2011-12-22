package world

class Job(queue: JobQueue, val profession:String = "General") {
  var block: Block = _
  var owner: Option[Mobile] = None
  def getObject = queue
  def work = ()
  def nextTask:Option[Task] = None

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.x) ~
      ("y" -> block.y) ~
      ("z" -> block.z) ~
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

class HarvestJob(queue:JobQueue) extends Job(queue, "Gardening") with WithID[Job] {
  override def nextTask = {
    if(owner.get.block.distanceFrom(block) >= 1)
      Some(new MoveToTask(owner.get,block,1))
    else if(block.plant.crop >0)
      Some(new HarvestTask(block.plant))
    else {
      queue.all -= this
      owner.get.job = None
      None
    }
  }
}

class CraftJob(queue:JobQueue, recipe:Recipe) extends Job(queue, "Crafting") with WithID[Job] {
  override def nextTask = {
    if( owner.get.fullfills(recipe.requirements)) {
      owner.get.craft(recipe)
      owner.get.job = None
      queue.all -= this
      None
    } else {
      println("doesn't fullfill")
      Some(new FindObjectTask(owner.get.unfullfilledObjects(recipe.requirements).head))
    }
  }

  override def toString = {
    "Craft: " + recipe
  }
}

class DigJob(queue:JobQueue) extends Job(queue, "Mining") with WithID[Job] {
  override def nextTask = {
    val task:Option[Task] = if(owner.get.block.distanceFrom(block) >= 2)
      Some(new MoveToTask(owner.get,block,2))
    else if(block.health > 0)
      Some(new AttackTask(block))
    else {
      queue.all -= this
      owner.get.job = None
      None
    }
    task
  }
}

class BuildJob(queue:JobQueue) extends Job(queue, "Building") with WithID[Job] {
  override def nextTask = {
    val task:Option[Task] =
      if(block.health == 100) {
        queue.all -= this
        owner.get.job = None
        None
      } else if(owner.get.objects.size < 1) {
        Object.availableObjects.headOption match {
          case Some(obj) => { Some(new FindObjectTask(obj.template)) }
          case None => { None }
        }
      }
      else if(owner.get.block.distanceFrom(block) >= 2) {
        Some(new MoveToTask(owner.get,block,2))}
      else
        Some(new BuildTask(block))
    task
  }
}

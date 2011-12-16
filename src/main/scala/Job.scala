package world

class Job(queue: JobQueue) {
  var block: Block = _
  var owner: Option[Mobile] = None
  def getObject = queue
  def work = ()
  def toJson = "{}"
  def nextTask:Option[Task] = None
}

class JobQueue(name:String) extends WithIDObject[Job] {
  import collection.mutable.ListBuffer

  def openJobs:ListBuffer[Job] = {
    all.filter(_.owner == None)
  }
  def findJob:Option[Job] = {
    openJobs.headOption
  }

  def toJson = {
    var js: String = "["
    js += all.map(j => j.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    js
  }
}
class DigJob(queue:JobQueue) extends Job(queue) with WithID[Job] {
  override def nextTask = {
    val task:Option[Task] = if(owner.get.distanceFrom(block) >= 2)
      Some(new MoveToTask(block))
    else if(block.health > 0)
      Some(new AttackTask(block))
    else {
      queue.all -= this
      owner.get.job = None
      None
    }
    task
  }

  override def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.x) ~
      ("y" -> block.y) ~
      ("z" -> block.z) ~
      ("type" -> "dig")
    compact(render(json))
  }
}

class BuildJob(queue:JobQueue) extends Job(queue) with WithID[Job] {
  override def nextTask = {
    val task:Option[Task] =
      if(block.health == 100) {
        queue.all -= this
        owner.get.job = None
        None
      } else if(owner.get.objects.size < 1)
        Some(new FindObject)
      else if(owner.get.distanceFrom(block) >= 2)
        Some(new MoveToTask(block))
      else
        Some(new BuildTask(block))
    task
  }

  override def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.x) ~
      ("y" -> block.y) ~
      ("z" -> block.z) ~
      ("type" -> "build")
    compact(render(json))
  }
}

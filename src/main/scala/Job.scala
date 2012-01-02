package net.geodesica

class Job(val queue: JobQueue, val profession:Profession = General, val requirements:Requirement = new Requirement)
  extends Ordered[Job]
{
  var block: Block = _
  var owner: Option[Mobile] = None
  var noTaskTimer = 0
  def getObject = queue
  def work = ()
  def finalTask:Option[Task] = None
  def finished = false

  def compare(that:Job) = noTaskTimer - that.noTaskTimer

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.coord.x) ~
      ("y" -> block.coord.y) ~
      ("z" -> block.coord.z) ~
      ("type" -> profession.toString)
    compact(render(json))
  }
}

class JobQueue(name:String) extends WithIDObject[Job] {
  import collection.mutable.ListBuffer

  def openJobs:ListBuffer[Job] = {
    all.filter(_.owner == None)
  }
  def findJob(professions:ListBuffer[Profession]):Option[Job] = {
    openJobs.filter(j => professions.contains(j.profession)).sorted.headOption
  }

  def toJson = {
    var js: String = "["
    js += all.filter(j => j.block != null).map(j => j.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    js
  }
}

class HarvestJob(queue:JobQueue) extends Job(queue, Gardening, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null || block.plant.crop == 0
  }
  override def finalTask = {
    Some(new HarvestTask(block.plant))
  }
}

class ClearJob(queue:JobQueue) extends Job(queue, WoodWorking, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null
  }

  override def finalTask = {
    Some(new ClearTask(block.plant))
  }
}

class CraftJob(queue:JobQueue, recipe:Recipe)
  extends Job(queue, Crafting, recipe.obj.requirements)
  with WithID[Job] {
  var newObject:Object = _
  override def finished = {
    newObject != null && newObject.health == 100
  }
  override def finalTask = {
    newObject = owner.get.craftInitial(recipe)
    owner.get.civilization.objects += newObject
    Some(new CraftTask(newObject))
  }

  override def toString = {
    "Craft: " + recipe
  }
}

class DigJob(queue:JobQueue) extends Job(queue, Mining, new Requirement(2)) with WithID[Job] {
  override def finished:Boolean = {
    block.health == 0
  }
  override def finalTask = {
    Some(new AttackTask(block))
  }
}

class BuildJob(queue:JobQueue) extends Job(queue, Building, new Requirement(2)) with WithID[Job] {
  override def finished = {
    block.health == 100
  }
  override def finalTask = {
    Some(new BuildTask(block))
  }
}

class ZoneStockpileJob(queue:JobQueue) extends Job(queue, Planning, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.adjacent.filter(b => b.zone == null).size == 0
  }

  override def finalTask = {
    Some(new ZoneStockpileTask(block))
  }
}

class InstallObjectJob(queue:JobQueue, ot:ObjectTemplate)
  extends Job(queue, General, new Requirement(0, List(new ConsumableRequirement(ot)),List()))
  with WithID[Job] {
  override def finished = {
    block.installedObject != null && block.installedObject.template == ot
  }

  override def finalTask = {
    owner.get.install(ot)
    None
  }

  override def toString = {
    "Install: "+ot+" At: "+block
  }
}

class CleanBlockJob(queue:JobQueue)
  extends Job(queue, General, new Requirement(0))
  with WithID[Job] {

  override def finished = {
    block.objects.size == 0
  }

  override def finalTask = {
    Some(new CleanBlockTask(block))
  }
}

class UnloadJob(queue:JobQueue)
  extends Job(queue, General, new Requirement(0)) 
  with WithID[Job] {

  override def finished = {
    owner.get.objects.size == 0
  }

  override def finalTask = {
    owner.get.objects.foreach(o => o.moveTo(block))
    None
  }
}

class ZoneHallJob(queue:JobQueue)
  extends Job(queue, Planning, new Requirement(0))
  with WithID[Job]
{
  override def finished = {
    !block.nearbyBlocks(4).exists({case (x,b) => b.zone == null})
  }

  override def finalTask = {
    Some(new ZoneHallTask(block,4))
  }
}

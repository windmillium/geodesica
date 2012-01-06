package net.geodesica

abstract class Job(val queue: JobQueue, val profession:Profession = General, val requirements:Requirement = new Requirement)
  extends Ordered[Job]
{
  val block:Block
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

class HarvestJob(val block:Block, queue:JobQueue) extends Job(queue, Gardening, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null || block.plant.crop == 0
  }
  override def finalTask = {
    Some(new HarvestTask(block.plant))
  }
}

class PlantJob(val block:Block, queue:JobQueue) extends Job(queue, Gardening, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant != null
  }

  override def finalTask = {
    Some(new PlantTask(block))
  }
}

class ClearJob(val block:Block, queue:JobQueue) extends Job(queue, WoodWorking, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null
  }

  override def finalTask = {
    Some(new ClearTask(block.plant))
  }
}

class CraftJob(val block:Block, queue:JobQueue, val recipe:Recipe)
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

class DigJob(val block:Block, queue:JobQueue) extends Job(queue, Mining, new Requirement(2)) with WithID[Job] {
  override def finished:Boolean = {
    block.health == 0
  }
  override def finalTask = {
    Some(new AttackTask(block))
  }
}

class BuildJob(val block:Block, queue:JobQueue) extends Job(queue, Building, new Requirement(2)) with WithID[Job] {
  override def finished = {
    block.health == 100
  }
  override def finalTask = {
    Some(new BuildTask(block))
  }
}

class InstallObjectJob(val block:Block, queue:JobQueue, ot:ObjectTemplate)
  extends Job(queue, General, new Requirement(1, List(new ConsumableRequirement(ot)),List()))
  with WithID[Job] {
  override def finished = {
    block.installedObject != null && block.installedObject.template == ot
  }

  override def finalTask = {
    owner.get.install(ot,block)
    None
  }

  override def toString = {
    "Install: "+ot+" At: "+block
  }
}

class CleanBlockJob(val block:Block, queue:JobQueue)
  extends Job(queue, General, new Requirement(0))
  with WithID[Job] {

  override def finished = {
    block.objects.size == 0
  }

  override def finalTask = {
    Some(new CleanBlockTask(block))
  }
}

class UnloadJob(val block:Block, queue:JobQueue)
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

object ZoneJob {
  def apply(kind:Symbol, desiredSize:Int, blocks:Set[Block], queue:JobQueue):Option[ZoneJob] = {
    val location = new SpaceFinder(blocks,desiredSize).location
    location match {
      case None => None
      case Some(block) => {
        val job = new ZoneJob(kind, block, desiredSize, queue)
        Some(job)
      }
    }
  }
}

class ZoneJob(kind:Symbol, val block:Block, desiredSize:Int, queue:JobQueue) extends Job(queue, Planning, new Requirement(0)) with WithID[Job] {
  override def finished = {
    !block.nearbyBlocks(desiredSize).flatten.exists(b => b.zone == null)
  }

  override def finalTask = {
    Some(new ZoneTask(kind, block, desiredSize))
  }
}

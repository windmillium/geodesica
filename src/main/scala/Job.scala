package net.geodesica

abstract class Job(val queue: JobQueue, val profession:Profession = General, val requirements:Requirement = new Requirement)
  extends Ordered[Job]
{
  val block:Block
  var owner: Option[Mobile] = None
  var noTaskTimer = 0
  def getObject = queue
  def work = ()
  def finalTask(mobile:Mobile):Option[Task]
  def finished = false
  def update(mobile:Mobile):Option[Job] = {
    if(finished) {
      queue.all -= this
      owner = None
      None
    } else if(noTaskTimer - mobile.jobTimer > 10){
      None
    } else {
      Some(this)
    }
  }

  def nextTask(mobile:Mobile):Option[Task] = {
    noTaskTimer += 1
    if(mobile.unfullfilledObjects(requirements.consumableRequirements).size > 0) {
      Some(new FindObjectTask(mobile.unfullfilledObjects(requirements.consumableRequirements).head))
    } else if( requirements.distance >= 0 && mobile.block.distanceFrom(block) > requirements.distance) {
      MoveToTask(mobile,block,requirements.distance)
    } else {
      finalTask(mobile)
    }
  }

  def compare(that:Job) = noTaskTimer - that.noTaskTimer

  def toJson = {
    import net.liftweb.json._
    import net.liftweb.json.JsonDSL._

    val json = ("x" -> block.coord.x) ~
      ("y" -> block.coord.y) ~
      ("z" -> block.coord.z) ~
      ("type" -> profession.toString) ~
      ("job" -> this.toString) ~
      ("timer" -> noTaskTimer)
    compact(render(json))
  }
}

class JobQueue extends WithIDObject[Job] {
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
  def finalTask(mobile:Mobile) = {
    Some(new HarvestTask(block.plant))
  }
}

class PlantJob(val block:Block, queue:JobQueue) extends Job(queue, Gardening, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant != null
  }

  def finalTask(mobile:Mobile) = {
    Some(new PlantTask(block))
  }
}

class ClearJob(val block:Block, queue:JobQueue) extends Job(queue, WoodWorking, new Requirement(0)) with WithID[Job] {
  override def finished = {
    block.plant == null
  }

  def finalTask(mobile:Mobile) = {
    Some(new ClearTask(block.plant))
  }

  override def toString = {
    "ClearJob("+id+"):" + block
  }
}

class CraftJob(val block:Block, queue:JobQueue, val recipe:Recipe)
  extends Job(queue, Crafting, recipe.obj.requirements)
  with WithID[Job] {
  var newObject:Object = _
  override def finished = {
    newObject != null && newObject.health == 100
  }
  def finalTask(mobile:Mobile) = {
//    Some(new WaitTask(10))
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
  def finalTask(mobile:Mobile) = {
    Some(new AttackTask(block))
  }
}

class BuildJob(val block:Block, queue:JobQueue) extends Job(queue, Building, new Requirement(2)) with WithID[Job] {
  override def finished = {
    block.health == 100
  }
  def finalTask(mobile:Mobile) = {
    Some(new BuildTask(block))
  }
}

class InstallObjectJob(val block:Block, queue:JobQueue, ot:ObjectTemplate)
  extends Job(queue, General, new Requirement(1, List(new ConsumableRequirement(ot)),List()))
  with WithID[Job] {
  override def finished = {
    block.installedObject != null && block.installedObject.template == ot
  }

  def finalTask(mobile:Mobile) = {
    owner.get.install(ot,block)
    Some(new WaitTask(1))
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

  def finalTask(mobile:Mobile) = {
    Some(new CleanBlockTask(block))
  }
}

class UnloadJob(val block:Block, queue:JobQueue)
  extends Job(queue, General, new Requirement(0)) 
  with WithID[Job] {

  override def finished = {
    owner.get.objects.size == 0
  }

  def finalTask(mobile:Mobile) = {
    owner.get.objects.foreach(o => o.moveTo(block))
    Some(new WaitTask(1))
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

  def finalTask(mobile:Mobile) = {
    Some(new ZoneTask(kind, block, desiredSize))
  }
}

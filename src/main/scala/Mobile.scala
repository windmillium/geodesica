package net.geodesica
import scala.collection.mutable.ListBuffer

object Mobile extends WithIDObject[Mobile] {
  def asJSON(startX: Int,startY: Int, width:Int,height:Int): String = {
    var js: String = "["
    js += all.filter({m => m.block.coord.x >= startX && m.block.coord.y >= startY && m.block.coord.x < startX+width && m.block.coord.y < startY+height}).map( m => m.asJSON).mkString(",").asInstanceOf[String]
    js += "]"
    return js
  }
}

trait Attackable {
  var health: Int
  def die

  def damage(amount: Int) = {
    import math._
    if(amount > 0)
      health = max(0, health-amount)
    else if(amount < 0)
      health = min(100, health-amount)

    if(health == 0)
      this.die
  }

}

class MobileSpecies(val name:String) {
  def create = {
    new Mobile(this)
  }
}

class Mobile(species:MobileSpecies = new MobileSpecies("Mobile"))
  extends WithID[Mobile]
  with Container
  with Attackable {
  var health = 100
  var civilization:Civilization = _
  var block: Block = _
  var job: Option[Job] = None
  var jobTimer:Int = 0
  var queue: JobQueue = _
  var task: Option[Task] = None
  import collection.mutable.HashMap
  val professions = new ListBuffer[Profession]
  professions += General

  def getObject = Mobile

  def fullfills(requirements:Requirement) = {
    unfullfilledObjects(requirements.consumableRequirements).size == 0 &&
    unfullfilledObjects(requirements.inventoryRequirements).size == 0
  }

  def unfullfilledObjects(requirements:List[ObjectRequirement]):List[ObjectTemplate] = {
    requirements.filter(r => objects.filter(o=>o.template == r.objectTemplate).size == 0).map(r => r.objectTemplate)
  }

  def assignCivilization(civilization:Civilization) = {
    this.civilization = civilization
    civilization.mobiles += this
  }

  def craftInitial(recipe:Recipe) = {
    val obj = recipe.obj.create
    objects += obj
    obj.health = 0
    recipe.obj.requirements.consumableRequirements.foreach(r => objects.filter(o => o.template == r.objectTemplate).head.destroy)
    obj
  }

  def placeObj(obj:Object) = {
    objects -= obj
    obj.moveTo(block)
  }

  def install(ot:ObjectTemplate,block:Block) = {
    val obj = objects.find(o => o.template == ot)
    obj match {
      case Some(obj) => {
        block.installedObject = obj
        obj.installed = block
        objects -= obj
        println(block.mobiles)
        val mobiles = block.mobiles.toList
        mobiles.foreach(m => m.moveTo(block.nearbyBlocks(3).flatten.filter(_.canAccept).headOption))
        println(block.mobiles)
      }
      case _ => ()
    }
  }

  def craft(obj:Attackable) {
    obj.damage(-25)
  }

  def attack(target:Attackable) {
    target.damage(25)
  }

  def build(target:Attackable) {
    target.damage(-25)
  }

  def clear(plant:Plant) {
    plant.damage(10)
  }

  def die = ()

  def updateJob:Option[Job] = {
    job.flatMap(_.update(this)).orElse(queue.findJob(professions).orElse(professions.headOption.flatMap(p => p.createJob(this))))
  }

  def updateTask:Option[Task] = {
    task.flatMap(_.nextStep(this)).orElse(job.flatMap(_.nextTask(this)))
  }

  def assignJob(newJob:Option[Job]) = {
    if(job != newJob){
    job match {
      case None => ()
      case Some(job) => job.owner = None
    }
    job = newJob
    newJob match {
      case None => ()
      case Some(nJob) => {
        nJob.owner = Some(this)
        this.jobTimer = nJob.noTaskTimer
      }
    }
    }
  }

  def update = {
    val job = updateJob
    assignJob(job)
    task = updateTask
    /*
          case Some(job) => {
            job.noTaskTimer += 1
            if(job.noTaskTimer - jobTimer > 10) {
              job.noTaskTimer = job.noTaskTimer * 10
              this.job = None
              job.owner = None
            } else
              this.task = nextTaskFor(job)
    */
  }

  def moveTowards(nblock:Block) = {
    if(block.coord.x > nblock.coord.x)
      move(3)
    else if(block.coord.x < nblock.coord.x)
      move(1)

    if(block.coord.y > nblock.coord.y)
      move(2)
    else if(block.coord.y < nblock.coord.y)
      move(0)
  }

  def move( direction: Int ):Boolean = {
    if(direction == -1)
      false
    else if(block != null){
      val add = direction match {
        case 0 => (0,1,0)
        case 1 => (1,0,0)
        case 2 => (0,-1,0)
        case 3 => (-1,0,0)
      }
      val nCoord = block.coord + add

      moveTo(block.blockAt(nCoord))
    } else
     false
  }

  def moveTo(newBlock: Option[Block]):Boolean = {
    newBlock match {
      case Some(nblock) => {
        if(nblock.canAccept){
          block.mobiles -= this
          nblock.mobiles += this
          block = nblock
          true
        } else
          false
      }
      case None => false
    }
  }

  def debugInfo:String = {
    var str:String = ""
    str += professions
    task match {
      case Some(task) => str += "task is"+task
      case None => str += "no task"
    }
    job match {
      case Some(job) => str += "job: " + job
      case None => str += "no job"
    }
    str += ", objects: "+objects
    str
  }

  def asJSON = {
    var str = "{"
    str += "\"id\":\""+id+"\","
    str += "\"x\":\""+block.coord.x+"\","
    str += "\"y\":\""+block.coord.y+"\","
    str += "\"z\":\""+block.coord.z+"\","
    str += "\"debug\":\""+debugInfo+"\""
    str += "}"
    str
  }
}

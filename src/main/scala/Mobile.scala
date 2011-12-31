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

class Mobile(species:MobileSpecies) extends WithID[Mobile] with Attackable {
  var health = 100
  var civilization:Civilization = _
  var block: Block = _
  var job: Option[Job] = None
  var queue: JobQueue = _
  var task: Option[Task] = None
  val objects = new ListBuffer[Object]
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

  def craftInitial(recipe:Recipe) = {
    val obj = recipe.obj.create
    objects += obj
    obj.health = 0
    recipe.obj.requirements.consumableRequirements.foreach(r => objects -= objects.filter(o => o.template == r.objectTemplate).head)
    obj
  }

  def createProfessionJob = {
    if(professions.contains(Planning)) {
      println("PLANNER")
      Planning.doWork(civilization)
    } else if(professions.contains(Crafting)) {
      var recipe:Option[Recipe] = None
      var i = 0
      while( recipe == None && i < civilization.recipes.size) {
        if( objects.filter(o => o.template == civilization.recipes.apply(i).obj ).size == 0)
          recipe = Some(civilization.recipes.apply(i))
        i += 1
      }

      if( recipe != None ) {
        val job = new CraftJob(queue,recipe.get)
        job.owner = Some(this)
        this.job = Some(job)
      } else {
        None
      }
    } else if(professions.contains(Gardening)) {
      if(Plant.all.filter(p => p.crop > 0).size > 0) {
        val block = Plant.all.filter(p=>p.crop > 0).head.block
        val job = new HarvestJob(this.queue)
        job.block = block
        this.job = Some(job)
        job.owner = Some(this)
      } else {
        None
      }
    } else if(professions.contains(WoodWorking)) {
      val blocks = WorldController.world.near(civilization.home,10)
      blocks.filter({case(coords,block)=>block.plant != null}).foreach(b => new ClearJob(queue).block = b._2)
    }
    else
      None
  }

  def findJob = {
    this.job = queue.findJob(professions)
    this.job match {
      case Some(job) => {
        job.owner = Some(this)
      }
      case None => createProfessionJob
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

  def update = {
    this.task match {
      case Some(task) => {
        this.task = task.nextStep(this)
      }
      case None => {
        this.job match {
          case None => {
            findJob
            if(block != null) {
              val rnd = new scala.util.Random
              val x = rnd.nextInt(10) - 5
              val y = rnd.nextInt(10) - 5
              val newCoord = civilization.home.coord+(x,y,0)
              val tBlock = block.blockAt(newCoord)
              if(tBlock != None && this.job == None)
                this.task = Some(new MoveToTask(this,tBlock.get,0))
            }
          }
          case Some(job) => {
            this.task = nextTaskFor(job)
          }
        }
      }
    }
  }

  def nextTaskFor(job:Job) = {
    if( job.finished ) {
      job.queue.all -= job
      this.job = None
      None
    } else if(unfullfilledObjects(job.requirements.consumableRequirements).size > 0) {
      Some(new FindObjectTask(unfullfilledObjects(job.requirements.consumableRequirements).head))
    } else if( job.requirements.distance >= 0 && block.distanceFrom(job.block) > job.requirements.distance) {
      Some(new MoveToTask(this,job.block,job.requirements.distance))
    } else {
      job.finalTask
    }
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

  def move( direction: Int ) = {
    if(block != null){
      val add = direction match {
        case 0 => (0,1,0)
        case 1 => (1,0,0)
        case 2 => (0,-1,0)
        case 3 => (-1,0,0)
      }
      val nCoord = block.coord + add

      val newBlock = for {
        newBlock <- block.blockAt(nCoord)
        newBlock <- newBlock.canAccept
      } yield newBlock

      moveTo(newBlock)
    }
  }

  def moveTo(newBlock: Option[Block]) = {
    newBlock match {
      case Some(nblock) => {
        block.mobiles -= this
        nblock.mobiles += this
        block = nblock
      }
      case None => None
    }
  }

  def debugInfo:String = {
    var str:String = ""
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

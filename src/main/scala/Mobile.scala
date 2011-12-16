package world
import scala.collection.mutable.ListBuffer

object Mobile extends WithIDObject[Mobile] {
  def asJSON(startX: Int,startY: Int, width:Int,height:Int): String = {
    var js: String = "["
    js += all.filter({m => m.block.x >= startX && m.block.y >= startY && m.block.x < startX+width && m.block.y < startY+height}).map( m => m.asJSON).mkString(",").asInstanceOf[String]
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

class Mobile extends WithID[Mobile] with Attackable {
  var health = 100
  var block: Block = _
  var job: Option[Job] = None
  var queue: JobQueue = _
  var task: Option[Task] = None
  val objects = new ListBuffer[Object]
  def getObject = Mobile

  def findJob = {
    this.job = queue.findJob
    this.job match {
      case Some(job) => {
        job.owner = Some(this)
      }
      case _ => ()
    }
  }

  def attack(target:Attackable) {
    target.damage(5)
  }

  def build(target:Attackable) {
    target.damage(-5)
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
            val rnd = new scala.util.Random
            this.move(rnd.nextInt(4))
          }
          case Some(job) => {
            this.task = job.nextTask
          }
        }
      }
    }
  }

  def distanceFrom(nblock: Block): Double = {
    import math._
    sqrt(pow(block.x - nblock.x, 2) + pow(block.y - nblock.y,2))
  }

  def moveTowards(nblock:Block) = {
    if(block.x > nblock.x)
      move(3)
    else if(block.x < nblock.x)
      move(1)

    if(block.y > nblock.y)
      move(2)
    else if(block.y < nblock.y)
      move(0)
  }

  def move( direction: Int ) = {
    val (x: Int, y: Int, z: Int) = direction match {
      case 0 => (block.x,block.y+1,block.z)
      case 1 => (block.x+1,block.y,block.z)
      case 2 => (block.x,block.y-1,block.z)
      case 3 => (block.x-1,block.y,block.z)
    }

    val newBlock = for {
      newBlock <- WorldController.world.blockAt(x,y,z)
      newBlock <- newBlock.canAccept(this)
    } yield newBlock

    moveTo(newBlock)
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

  def asJSON = {
    var str = "{"
    str += "\"id\":\""+id+"\","
    str += "\"x\":\""+block.x+"\","
    str += "\"y\":\""+block.y+"\","
    str += "\"z\":\""+block.z+"\""
    str += "}"
    str
  }
}

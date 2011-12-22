package world

trait Task {
  def nextStep(mobile:Mobile):Option[Task]
}

class WaitTask(var time:Int) extends Task {
  def nextStep(mobile:Mobile) = {
    time -= 1

    if(time <= 0)
      None
    else
      Some(this)
  }
}

class BuildTask(target:Attackable) extends Task {
  def nextStep(mobile:Mobile) = {
    mobile.build(target)
    if(target.health < 100)
      Some(this)
    else
      mobile.objects.remove(0)
      None
  }
}
class AttackTask(target:Attackable) extends Task {
  def nextStep(mobile:Mobile) = {
    mobile.attack(target)
    if(target.health > 0)
      Some(this)
    else
      None
  }
}

class HarvestTask(plant:Plant) extends Task {
  def nextStep(mobile:Mobile) = {
    if(plant.crop > 0) {
      plant.crop -= 1
      mobile.objects += plant.cropTemplate.create
      Some(this)
    } else
     None 
  }
}

class MoveToTask(mobile:Mobile, block:Block, distance:Int) extends Task {
  var path = (new AStarSearch[Block]).search(block,mobile.block)


  def nextStep(mobile:Mobile):Option[Task] = {

    if(path == null || path.size == 0) {
      return Some(new WaitTask(10))
    }

    val dblock = path.last
    path = path.dropRight(1)

    if(mobile.block.x > dblock.x)
      mobile.move(3)
    else if(mobile.block.x < dblock.x)
      mobile.move(1)

    if(mobile.block.y > dblock.y)
      mobile.move(2)
    else if(mobile.block.y < dblock.y)
      mobile.move(0)

    if(mobile.block.distanceFrom(block) >= distance)
      Some(this)
    else
      None
  }
}

class FindObjectTask(obj:ObjectTemplate) extends Task {
  def nextStep(mobile:Mobile) = {
    val relevantObjects = mobile.block.objects.filter(o => o.template == obj)
    val worldObjects = Object.all.filter(o=>o.template == obj && o.block != null)

    if(relevantObjects.size > 0) {
      val obj = relevantObjects.head
      mobile.block.objects -= obj
      obj.block = null
      mobile.objects += obj
      None
    } else if(worldObjects.size > 0) {
      Some(new MoveToTask(mobile,worldObjects.head.block, 0))
    } else {
      None
    }
  }

  override def toString = {
    "FindObject: " + obj
  }
}


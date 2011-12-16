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

class MoveToTask(block:Block) extends Task {
  def nextStep(mobile:Mobile) = {
    if(mobile.block.x > block.x)
      mobile.move(3)
    else if(mobile.block.x < block.x)
      mobile.move(1)

    if(mobile.block.y > block.y)
      mobile.move(2)
    else if(mobile.block.y < block.y)
      mobile.move(0)

    if(mobile.distanceFrom(block) >= 2)
      Some(this)
    else
      None
  }
}

class FindObject extends Task {
  def nextStep(mobile:Mobile) = {
    Object.availableObjects.headOption match {
      case None => {
        println("no object")
        Some(this)
      }
      case Some(obj) => {
        if(mobile.block.objects.size > 0) {
          val obj = mobile.block.objects.remove(0)
          obj.block = null
          mobile.objects += obj

          None
        } else {
          Some(new MoveToTask(obj.block))
        }
      }
    }
  }
}


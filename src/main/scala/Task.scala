package net.geodesica

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

class CraftTask(obj:Object) extends Task {
  def nextStep(mobile:Mobile) = {
    mobile.craft(obj)
    if(obj.health < 100)
      Some(this)
    else {
      mobile.placeObj(obj)
      None
    }
  }
}

class ClearTask(plant:Plant) extends Task {
  def nextStep(mobile:Mobile) = {
    mobile.clear(plant)
    if(plant.health < 1)
      None
    else
      Some(this)
  }
}

class HarvestTask(plant:Plant) extends Task {
  def nextStep(mobile:Mobile) = {
    if(plant.crop > 0) {
      plant.crop -= 1
      val obj = plant.cropTemplate.create
      mobile.objects += obj
      mobile.civilization.objects += obj
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

    if(mobile.block.coord.x > dblock.coord.x)
      mobile.move(3)
    else if(mobile.block.coord.x < dblock.coord.x)
      mobile.move(1)

    if(mobile.block.coord.y > dblock.coord.y)
      mobile.move(2)
    else if(mobile.block.coord.y < dblock.coord.y)
      mobile.move(0)

    if(mobile.block.distanceFrom(block) >= distance)
      Some(this)
    else
      None
  }
}

class FindObjectTask(obj:ObjectTemplate) extends Task {
  def nextStep(mobile:Mobile) = {
    val relevantObjects = mobile.block.freeObjects.filter(o => o.template == obj)
    val worldObjects = mobile.civilization.freeObjects.filter(o => o.template == obj)

    if(relevantObjects.size > 0) {
      relevantObjects.head.moveTo(mobile)
      None
    } else if(worldObjects.size > 0) {
      Some(new MoveToTask(mobile,worldObjects.head.container.block, 0))
    } else {
      None
    }
  }

  override def toString = {
    "FindObject: " + obj
  }
}

class ZoneStockpileTask(block:Block) extends Task {
  def nextStep(mobile:Mobile) = {
    val sp = new Stockpile
    mobile.civilization.zones += sp
    sp.blocks ++= block.adjacent
    block.adjacent.foreach(b => b.zone = sp)
    sp.requirements.map({case (b,Some(o)) => mobile.civilization.recipes.find(r => r.obj == o )}).flatten.foreach(r => new CraftJob(mobile.queue,r))
    println(sp.requirements.map({case (b,o) => mobile.civilization.recipes.find(r => {println(o);println(r.obj);r.obj == o} )}))
    sp.requirements.foreach({case (b,o) => new InstallObjectJob(mobile.queue, o.get).block = b})
    println(mobile.queue.all)
    None
  }
}

class ZoneHallTask(block:Block, size:Int) extends Task {
  def nextStep(mobile:Mobile) = {
    val hall = new Hall
    mobile.civilization.zones += hall

    val blocks = block.nearbyBlocks(size).collect({case(x,b) => b})
    hall.blocks ++= blocks
    blocks.foreach(b => b.zone = hall)

    println(hall.requirements)
    println(hall.requirements.map({case (b,Some(o)) => mobile.civilization.recipes.find(r => r.obj == o )}))
    println(hall.blocks.enclosingBlocks)
    hall.requirements.toList.map({case (b,Some(o)) => mobile.civilization.recipes.find(r => r.obj == o )}).flatten.foreach(r => new CraftJob(mobile.queue,r))
    hall.requirements.foreach({case (b,o) => new InstallObjectJob(mobile.queue, o.get).block = b})
    println(mobile.queue.all)
    None
  }
}

class CleanBlockTask(block:Block) extends Task {
  def nextStep(mobile:Mobile) = {
    block.objects.foreach(o => o.moveTo(mobile))
    None
  }
}


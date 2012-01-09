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
    if(plant.health < 1)
      None
    else {
      mobile.clear(plant)
      Some(this)
    }
  }
}

object HarvestTask {
  def apply(plant:Plant) = {
    Some(new HarvestTask(plant))
  }
}

class HarvestTask(plant:Plant) extends Task {
  def nextStep(mobile:Mobile) = {
    if(mobile.block != plant.block)
      MoveToTask(mobile, plant.block, 0)
    else if(plant.crop > 0) {
      plant.crop -= 1
      val obj = plant.cropTemplate.create
      mobile.objects += obj
      mobile.civilization.objects += obj
      Some(this)
    } else
     None
  }
}

object MoveToTask {
  def apply(mobile:Mobile, block:Block, distance:Int) = {
    var path = new AStarSearch[Block].search(block,mobile.block)
    if(path == null)
      None
    else {
      path = path.dropRight(1)
      path = path.drop(distance)
      Some(new MoveToTask(mobile,block,distance,path))
    }
  }
}

class MoveToTask(mobile:Mobile, block:Block, distance:Int, var path:List[Block]) extends Task {
  def nextStep(mobile:Mobile):Option[Task] = {

    if(path.size == 0 || mobile.block.distanceFrom(block) < distance) {
      return None
    }

    val dblock = path.last
    path = path.dropRight(1)

    val direction:Int = if(mobile.block.coord.x > dblock.coord.x)
      3
    else if(mobile.block.coord.x < dblock.coord.x)
      1
    else if(mobile.block.coord.y > dblock.coord.y)
      2
    else if(mobile.block.coord.y < dblock.coord.y)
      0
    else
      -1

    if(mobile.move(direction))
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
      MoveToTask(mobile,worldObjects.head.container.block, 0)
    } else {
      None
    }
  }

  override def toString = {
    "FindObject: " + obj
  }
}

class ZoneTask(kind:Symbol, block:Block, size:Int) extends Task {
  def nextStep(mobile:Mobile) = {
    val zone = kind match {
      case 'Stockpile => new Stockpile
      case 'Workshop => new Workshop
      case 'Hall => new Hall
      case 'Home => new Home
      case 'Farm => new Farm
    }

    mobile.civilization.zones += zone

    val blocks = block.nearbyBlocks(size).flatten
    zone.blocks ++= blocks
    blocks.foreach(b => b.zone = zone)
    zone.requirements.toList.flatMap(r => r._2).map(o => mobile.civilization.recipes.find(r => r.obj == o )).flatten.foreach(r =>
      new CraftJob(mobile.block,mobile.queue,r))
    zone.requirements.foreach({case (b,o) => new InstallObjectJob(b,mobile.queue, o.get)})
    None
  }
}

class CleanBlockTask(block:Block) extends Task {
  def nextStep(mobile:Mobile) = {
    block.objects.foreach(o => o.moveTo(mobile))
    mobile.assignJob(Some(new UnloadJob(mobile.civilization.stockpiles.head.blocks.head, mobile.queue)))
    None
  }
}

object PlantTask {
  def apply(farm:Farm) = {
    println(farm)
    farm.freeBlock.map(new PlantTask(_))
  }
}

class PlantTask(block:Block) extends Task {
  def nextStep(mobile:Mobile) = {
    if(block != mobile.block)
      MoveToTask(mobile, block, 0 )
    else {
      val plant = PlantSpecies("Bush").get.create(block)
      block.plant = plant
      None
    }
  }
}



// class FindSpaceTask(size:Int, job:ZoneHomeJob) extends Task {
//   def nextStep(mobile:Mobile) = {
//     val blocks = mobile.civilization.blocks.map({case(c,b) => b}).toSet
//     val sf = new SpaceFinder(blocks, size)
//     val location = sf.location
//     location match {
//       case None => None
//       case Some(nblock) => {
//         job.location = nblock
//         job.block = nblock
//         None
//       }
//     }
//   }
// }


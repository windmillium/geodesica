package net.geodesica

trait Profession

object Mining extends Profession {
  override def toString = {
    "Mining"
  }
}
object Building extends Profession {
  override def toString = {
    "Building"
  }
}
object Gardening extends Profession {
  override def toString = {
    "Gardening"
  }
}
object Crafting extends Profession {
  override def toString = {
    "Crafting"
  }
}
object WoodWorking extends Profession {
  override def toString = {
    "WoodWorking"
  }
}
object Planning extends Profession {
  def doWork(mobile:Mobile) = {
    val civilization = mobile.civilization
    val cleaningBlocks = civilization.queue.all.collect({case(j:CleanBlockJob) => j.block})
    civilization.blocks.filter({case(x,b) => b.zone == null && b.objects.size > 0 && !cleaningBlocks.contains(b)}).foreach({ case(x,b) =>
      new CleanBlockJob(civilization.queue).block = b
    })
    if(civilization.stockpiles.size == 0 && !civilization.queue.all.exists(j => j.isInstanceOf[ZoneStockpileJob] )) {
      val blocks = civilization.blocks.map({case(c,b) => b}).toSet
      val location = new SpaceFinder(blocks, 1).location
      mobile.assignJob(ZoneStockpileJob(location, 1, civilization.queue))
    }
    else if(civilization.halls.size == 0){
      val blocks = civilization.blocks.map({case(c,b) => b}).toSet
      val location = new SpaceFinder(blocks, 3).location
      mobile.assignJob(ZoneHallJob(location, 3, civilization.queue))
    }
    else if(civilization.workshops.size == 0){
      val blocks = civilization.blocks.map({case(c,b) => b}).toSet
      val location = new SpaceFinder(blocks, 2).location
      mobile.assignJob(ZoneWorkshopJob(location, 2, civilization.queue))
    }
    else
      None
  }
  override def toString = {
    "Planning"
  }
}

object General extends Profession {
  def doWork(mobile:Mobile) = {
    if(mobile.objects.size > 0 && mobile.civilization.stockpiles.size > 0) {
      val njob = new UnloadJob(mobile.queue)
      njob.block = mobile.civilization.stockpiles.head.blocks.head
      njob.owner = Some(mobile)
      mobile.job = Some(njob)
      None
    }
  }
  override def toString = {
    "General"
  }
}


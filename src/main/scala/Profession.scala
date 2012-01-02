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
  def doWork(civilization:Civilization) = {
    val cleaningBlocks = civilization.queue.all.collect({case(j:CleanBlockJob) => j.block})
    civilization.blocks.filter({case(x,b) => b.zone == null && b.objects.size > 0 && !cleaningBlocks.contains(b)}).foreach({ case(x,b) =>
      new CleanBlockJob(civilization.queue).block = b
    })
    if(civilization.stockpiles.size == 0)
      Some(new ZoneStockpileJob(civilization.queue).block = civilization.home.blockAt(civilization.home.coord+(0,0,0)).get)
    else if(civilization.halls.size == 0){
      Some(new ZoneHallJob(civilization.queue).block = civilization.home.blockAt(civilization.home.coord+(0,9,0)).get)
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


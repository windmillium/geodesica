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
    if(civilization.stockpiles.size == 0)
      Some(new ZoneStockpileJob(civilization.queue).block = civilization.home.blockAt(civilization.home.coord+(0,0,0)).get)
    else if(!civilization.stockpiles.head.finished){
      println(civilization.stockpiles.head.objects)
      // Some(civilization.stockpiles.head.
      None
    }
    else
      None
  }
  override def toString = {
    "Planning"
  }
}
object General extends Profession


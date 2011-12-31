package net.geodesica

trait Profession

object Mining extends Profession
object Building extends Profession
object Gardening extends Profession
object Crafting extends Profession
object WoodWorking extends Profession
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
}
object General extends Profession


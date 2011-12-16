package world

import scala.collection.mutable.HashMap

object WorldController {

  var world : World = _

  def main(args: Array[String]) = {
    // if( args.length < 1 ) {
    //   println("run it like this: geodesica 100")
    //   sys.exit(0)
    // }
    // world = new World(args(0).toInt)
    world = new World(10)
    var x : Int = 1
    while( x != 0 ) {
      world.update
      Thread.sleep(100)
    }
  }

  def asJSON(startX: Int, startY: Int, width:Int,height:Int) = {
    world.asJSON(startX,startY,width,height)
  }
}

class World( val height: Int, val depth: Int = 1) {
  val width = height * 2
  val map = HashMap.empty[(Int,Int,Int), Block]
  val queue = new JobQueue("playerQueue")

  createWorld
  seedPlantsAndAnimals

  import akka.camel.CamelServiceManager._
  import akka.actor.{Actor,ActorRef}

  startCamelService

  val mobileActor = Actor.actorOf[MobileActor]
  val myActor = Actor.actorOf[MyActor]
  val buildingActor = Actor.actorOf[BuildingActor]
  val jobActor = Actor.actorOf(new JobActor(queue))

  mobileActor.start
  myActor.start
  buildingActor.start
  jobActor.start

  def indexFor( x : Int, y : Int, z : Int ) = {
    x + y * width + z * width * height
  }

  def blockAt( x : Int, y : Int, z : Int ): Option[Block] = {
    return map.get((x,y,z))
  }

  def createWorld = {
    for( z <- 0 until depth ) {
      for( y <- 0 until height ) {
        for( x <- 0 until width ) {
          val index = indexFor(x,y,z)
          val health = if( x < width/2 ) 0; else 100;
          map += ( (x,y,z) -> new Block(x,y,z,health))
        }
      }
    }

    println("Total size: %s".format(map.size))
  }

  def seedPlantsAndAnimals() = {
    val rnd = new scala.util.Random

    for(block <- map if(rnd.nextInt(100) > 95 && block._2.health == 0)) {
      val plant = new Plant
      block._2.plant = plant

      val mob = new Mobile
      mob.queue = queue
      mob.block = block._2
      block._2.mobiles += mob
    }

    println("Plants: %s".format(Plant.all.size))
    println("Mobiles: %s".format(Mobile.all.size))
  }

  def update = {
  //  Plant.update
    Mobile.all.foreach(mobile => mobile.update)
  }

  def asJSON(startX: Int,startY: Int, width:Int,height:Int): String = {
    var js : String = "["
    js += map.
      filterKeys({b => b._1 >= startX && b._2 >= startY && b._1 < startX+width && b._2 < startY+height}).
      map( w => w._2.json).mkString(",").asInstanceOf[String]
    js += "]"
    return js
  }
}


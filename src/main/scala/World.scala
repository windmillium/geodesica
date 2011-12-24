package net.geodesica

import scala.collection.mutable.HashMap

object WorldController {

  var world : World = _

  def main(args: Array[String]) = {
    // if( args.length < 1 ) {
    //   println("run it like this: geodesica 100")
    //   sys.exit(0)
    // }
    // world = new World(args(0).toInt)
    world = new World(15)
    var x : Int = 1
    while( x != 0 ) {
      world.update
      Thread.sleep(250)
    }
  }

  def asJSON(startX: Int, startY: Int, width:Int,height:Int) = {
    world.asJSON(startX,startY,width,height)
  }
}

class World( val height: Int, val depth: Int = 1) {
  val width = height * 2
  val map = HashMap.empty[(Int,Int,Int), Block]

  val player = new Civilization("Player")
  val wilderness = new Civilization("Wilderness")

  loadObjectTemplates
  createWorld
  seedPlantsAndAnimals

  import akka.camel.CamelServiceManager._
  import akka.actor.{Actor,ActorRef}

  startCamelService

  val mobileActor = Actor.actorOf[MobileActor]
  val myActor = Actor.actorOf[MyActor]
  val templateActor = Actor.actorOf[BuildingTemplateActor]
  val jobActor = Actor.actorOf(new JobActor(player.queue))

  mobileActor.start
  myActor.start
  templateActor.start
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
          map += ( (x,y,z) -> new Block(x,y,z,health, ObjectTemplate.all.head))
        }
      }
    }

    println("Total size: %s".format(map.size))
  }

  def seedPlantsAndAnimals() = {

    val rnd = new scala.util.Random

    val tree = new PlantSpecies("tree")
    tree.cropTemplate = new ObjectTemplate("Stick")

    for(block <- map if(rnd.nextInt(100) > 85 && block._2.health == 0)) {
      val plant = tree.create(block._2)
      block._2.plant = plant
    }

    val human = new MobileSpecies("human")
    val deer = new MobileSpecies("deer")
    val centerBlock = blockAt(width/4,height/2,0).get
    var x = 10
    while(x > 0) {
      val mob = human.create
      if( x == 10)
        mob.professions += "Mining"
      if(x == 9)
        mob.professions += "Building"
      if(x == 8)
        mob.professions += "GGardening"
      if(x == 7)
        mob.professions += "Crafting"

      mob.civilization = player
      mob.queue = player.queue
      mob.block = centerBlock
      centerBlock.mobiles += mob
      x -= 1
    }
    x = 5
    while(x > 0) {
      val mob = deer.create
      mob.civilization = wilderness
      mob.queue = wilderness.queue
      mob.block = centerBlock
      centerBlock.mobiles += mob
      x -= 1
    }

    println("Plants: %s".format(Plant.all.size))
    println("Mobiles: %s".format(Mobile.all.size))
  }

  import collection.mutable.ListBuffer
  var path:List[Block] = _
  def update = {
    Plant.update
    var selected = map.filter({b => b._2.selected == true})
    if(selected.size == 2) {
      val search = new AStarSearch[Block]
      path = search.search(selected.head._2, selected.last._2)
      selected.head._2.selected = false
      selected.last._2.selected = false
    }

    if(path != null && path.size > 0) {
      path.head.classes += "path"
      path = path.drop(1)
    }

    Mobile.all.foreach(mobile => mobile.update)
  }

  def loadObjectTemplates = {
    val rubble = new ObjectTemplate("Rubble")
    val rh = new ObjectTemplate("Rock Hammer")
    player.recipes += new Recipe(rh, List(new Requirement(rubble)))
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


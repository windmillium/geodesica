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
    world = new World(30)
    world.startWeb
    var x : Int = 1
    while( x != 0 ) {
      world.update
      Thread.sleep(250)
    }
  }
}

class BlockMap {
  val blocks = HashMap.empty[(Int,Int,Int), Block]

  def blockAt(coord:Coord): Option[Block] = {
    return blocks.get((coord.x,coord.y,coord.z))
  }

  def asJSON(startX: Int,startY: Int, width:Int,height:Int): String = {
    var js : String = "["
    js += blocks.
      filterKeys({b => b._1 >= startX && b._2 >= startY && b._1 < startX+width && b._2 < startY+height}).
      map( w => w._2.toJson).mkString(",").asInstanceOf[String]
    js += "]"
    return js
  }
}

class World( val height: Int, val depth: Int = 1) {
  val width = height * 2
  val blockMap = new BlockMap

  val player = new Civilization("Player")
  val wilderness = new Civilization("Wilderness")

  loadObjectTemplates

  createWorld(width,height,depth)
  seedPlantsAndAnimals

  def startWeb = {
    import akka.camel.CamelServiceManager._
    import akka.actor.{Actor,ActorRef}

    startCamelService

    val mobileActor = Actor.actorOf[MobileActor]
    val blockActor = Actor.actorOf(new BlockMapActor(blockMap))
    val templateActor = Actor.actorOf[BuildingTemplateActor]
    val jobActor = Actor.actorOf(new JobActor(player.queue, blockMap))

    mobileActor.start
    blockActor.start
    templateActor.start
    jobActor.start
  }

  def near(block:Block,distance:Int) = {
    blockMap.blocks.filterKeys({case(x,y,z) => x < block.coord.x+distance && x > block.coord.x-distance && y < block.coord.y+distance && y > block.coord.y-distance})
  }

  def createWorld(width:Int,height:Int,depth:Int) = {
    for( z <- 0 until depth ) {
      for( y <- 0 until height ) {
        for( x <- 0 until width ) {
          val health = if( x < width/2 ) 0; else 100;
          new Block(blockMap, new Coord(x,y,z),health, ObjectTemplate.all.head)
        }
      }
    }

    println("Total size: %s".format(blockMap.blocks.size))
  }


  def seedPlantsAndAnimals() = {
    wilderness.home = blockMap.blockAt(new Coord(0,0,0)).get
    player.home = blockMap.blockAt(new Coord(width/4,height/2,0)).get

    val rnd = new scala.util.Random

    val cot = new ObjectTemplate("Stick")
    val dot = new ObjectTemplate("Wood")
    val tree = new PlantSpecies("tree",cot,dot)

    for(block <- blockMap.blocks if(rnd.nextInt(100) > 85 && block._2.health == 0)) {
      val plant = tree.create(block._2)
      block._2.plant = plant
    }

    val human = new MobileSpecies("human")
    val deer = new MobileSpecies("deer")
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
      if(x == 6)
        mob.professions += "WoodWorking"

      mob.civilization = player
      mob.queue = player.queue
      mob.block = player.home
      player.home.mobiles += mob
      x -= 1
    }
    x = 5
    while(x > 0) {
      val mob = deer.create
      mob.civilization = wilderness
      mob.queue = wilderness.queue
      mob.block = wilderness.home
      wilderness.home.mobiles += mob
      x -= 1
    }

    println("Plants: %s".format(Plant.all.size))
    println("Mobiles: %s".format(Mobile.all.size))
  }

  import collection.mutable.ListBuffer
  var path:List[Block] = _
  def update = {
    Plant.update
    var selected = blockMap.blocks.filter({b => b._2.selected == true})
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

    val rhRequirement = new Requirement(-1,List(new ConsumableRequirement(rubble)))
    val rh = new ObjectTemplate("Rock Hammer", rhRequirement)
    val drhRequirements = new Requirement(-1,List(new ConsumableRequirement(rh)),List(new InventoryRequirement(rh)))
    val drh = new ObjectTemplate("Double Rock Hammer", drhRequirements)
    player.recipes += new Recipe(rh)
    player.recipes += new Recipe(drh)
  }

}


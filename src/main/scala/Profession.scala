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
  def createJob(mobile:Mobile):Option[Job] = {
    mobile.civilization.blocks.flatten.filter(b => b.plantWithCrop).headOption.map(new HarvestJob(_,mobile.queue)).
    orElse(mobile.civilization.farms.headOption.flatMap(f => f.freeBlock).map(new PlantJob(_,mobile.queue)))
  }

  override def toString = {
    "Gardening"
  }
}
object Crafting extends Profession {
  def createJob(mobile:Mobile) = {
    var recipe:Option[Recipe] = None
    var i = 0
    while( recipe == None && i < mobile.civilization.recipes.size) {
      if( mobile.civilization.objects.filter(o => o.template == mobile.civilization.recipes.apply(i).obj ).size == 0)
        if(mobile.civilization.queue.all.collect({case(j:CraftJob) => j }).filter({case(j:CraftJob) => j.recipe == mobile.civilization.recipes.apply(i)}).size ==0)
          recipe = Some(mobile.civilization.recipes.apply(i))
      i += 1
    }

    if( recipe != None ) {
      Some(new CraftJob(mobile.block,mobile.queue,recipe.get))
    } else {
      None
    }
  }

  override def toString = {
    "Crafting"
  }
}
object WoodWorking extends Profession {
  def createJob(mobile:Mobile) = {
    val blocks = mobile.civilization.blocks
    blocks.flatten.filter(block => block.plant != null && (block.zone == null || !block.zone.isInstanceOf[Farm] )).headOption.map(b => new ClearJob(b,mobile.queue))
  }

  override def toString = {
    "WoodWorking"
  }
}

object Planning extends Profession {
  def createJob(mobile:Mobile):Option[Job] = {
    val civilization = mobile.civilization
    val cleaningBlocks = civilization.queue.all.collect({case(j:CleanBlockJob) => j.block})
    civilization.blocks.flatten.filter(b => (b.zone == null || !b.zone.isInstanceOf[Stockpile]) && b.objects.size > 0 && !cleaningBlocks.contains(b)).
      foreach(b => new CleanBlockJob(b,civilization.queue))

  civilization.nextZone.flatMap({case(kind,size) => ZoneJob(kind,size, civilization.blocks.flatten.toSet, civilization.queue)})
  }

  override def toString = {
    "Planning"
  }
}

object General extends Profession {
  def doWork(mobile:Mobile) = {
    if(mobile.objects.size > 0 && mobile.civilization.stockpiles.size > 0) {
      val njob = new UnloadJob(mobile.civilization.stockpiles.head.blocks.head,mobile.queue)
      njob.owner = Some(mobile)
      mobile.job = Some(njob)
      None
    }
  }
  override def toString = {
    "General"
  }
}


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
  def createJob(mobile:Mobile) = {
    if(Plant.all.filter(p => p.crop > 0).size > 0) {
      val block = Plant.all.filter(p=>p.crop > 0).head.block
      val job = new HarvestJob(mobile.queue)
      job.block = block
      mobile.job = Some(job)
      job.owner = Some(mobile)
    } else {
      None
    }
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
      val job = new CraftJob(mobile.queue,recipe.get)
      job.owner = Some(mobile)
      mobile.job = Some(job)
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
    blocks.filter({case(coords,block)=>block.plant != null}).foreach(b => new ClearJob(mobile.queue).block = b._2)
  }

  override def toString = {
    "WoodWorking"
  }
}

object Planning extends Profession {
  def createJob(mobile:Mobile):Option[Job] = {
    val civilization = mobile.civilization
    val cleaningBlocks = civilization.queue.all.collect({case(j:CleanBlockJob) => j.block})
    civilization.blocks.filter({case(x,b) => (b.zone == null || !b.zone.isInstanceOf[Stockpile]) && b.objects.size > 0 && !cleaningBlocks.contains(b)}).foreach({ case(x,b) =>
      new CleanBlockJob(civilization.queue).block = b
    })

  civilization.nextZone.flatMap({case(kind,size) => ZoneJob(kind,size, civilization.blocks.map({case(c,b) => b}).toSet, civilization.queue)})
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


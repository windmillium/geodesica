package net.geodesica

object Plant extends WithIDObject[Plant] {
  def update = {
    all.foreach(p => p.update)
  }
}

class Plant(val species:PlantSpecies, val block:Block) extends WithID[Plant] with Attackable {
  var age: Int = 0
  var crop: Int = 1
  var health = 100
  def cropTemplate = species.cropTemplate
  def deathTemplate = species.deathTemplate
  def getObject = Plant

  def update = {
    age += 1
    if(age % 10000 == 0)
      crop += 1
  }

  def die = {
    block.plant = null
    Plant.all -= this
    for( x <- 0 until 10)
      deathTemplate.create.moveTo(block)
  }
}

object PlantSpecies extends WithIDObject[PlantSpecies] {
  def apply(name:String) = {
    all.find(p => p.name == name)
  }
}
class PlantSpecies(val name:String, val cropTemplate:ObjectTemplate, val deathTemplate:ObjectTemplate) extends WithID[PlantSpecies] {
  def create(block:Block) = {
    new Plant(this,block)
  }

  def getObject = PlantSpecies
}

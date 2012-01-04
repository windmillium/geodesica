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
    for( x <- 0 until 2)
      deathTemplate.create.moveTo(block)
  }
}

class PlantSpecies(val name:String, val cropTemplate:ObjectTemplate, val deathTemplate:ObjectTemplate) {
  def create(block:Block) = {
    new Plant(this,block)
  }
}

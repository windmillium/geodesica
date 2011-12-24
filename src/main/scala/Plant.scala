package world

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
  def getObject = Plant

  def update = {
    age += 1
    if(age % 1000 == 0)
      crop += 1
  }

  def die = {
    block.plant = null
  }
}

class PlantSpecies(name:String) {
  var cropTemplate:ObjectTemplate = _
  def create(block:Block) = {
    new Plant(this,block)
  }
}

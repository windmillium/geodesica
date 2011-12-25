package net.geodesica

class Requirement(
  val distance:Int = -1,
  val consumableRequirements:List[ConsumableRequirement] = List(),
  val inventoryRequirements:List[InventoryRequirement] = List()
) {

  override def toString = {
    "Requires: "+consumableRequirements+", "+inventoryRequirements
  }
}

trait ObjectRequirement {
  def objectTemplate:ObjectTemplate
}

class ConsumableRequirement(val objectTemplate:ObjectTemplate) extends ObjectRequirement {
  override def toString = {
    "ConsumableRequirement: "+objectTemplate
  }
}

class InventoryRequirement(val objectTemplate:ObjectTemplate) extends ObjectRequirement {
  override def toString = {
    "InventoryRequirement: "+objectTemplate
  }
}

class Recipe(val obj:ObjectTemplate, val requirements:Requirement) {
  override def toString = {
    "Recipe: " +obj+", Requires: "+requirements
  }
}

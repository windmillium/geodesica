package world

class Requirement(val objectTemplate:ObjectTemplate) {
  override def toString = {
    "Requirement: "+objectTemplate.toString
  }
}

class Recipe(val obj:ObjectTemplate, val requirements:List[Requirement]) {
  override def toString = {
    "Recipe: " +obj+", Requires: "+requirements
  }
}

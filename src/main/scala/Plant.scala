package world

object Plant extends WithIDObject[Plant]

class Plant extends WithID[Plant] {
  var age: Int = 0

  def getObject = Plant

  def update = {
    age += 1
    println(age)
  }
}

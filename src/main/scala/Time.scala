package world

class IntWrapper(i:Int) {
  def milliseconds = i
  def seconds = i * 1000
  def minutes = i * 60000
  def hours = i * 3600000
  def days = i * 86400000
}

object TimeWrapper { implicit def wrapInt(i:Int) = new IntWrapper(i) }

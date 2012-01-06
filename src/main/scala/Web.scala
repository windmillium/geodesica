package net.geodesica

import akka.actor.{Actor, ActorRef}
import akka.camel.{Message, Consumer}

class MobileActor extends Actor with Consumer {
  def endpointUri = "jetty:http://localhost:8888/mobiles"

  def receive = {
    case msg: Message =>
      val coords = msg.headers(Set("startx","starty","width","height"))
      val viewport = (
        coords.get("startx").getOrElse("0" ).asInstanceOf[String].toInt,
        coords.get("starty").getOrElse("0" ).asInstanceOf[String].toInt,
        coords.get("width" ).getOrElse("20").asInstanceOf[String].toInt,
        coords.get("height").getOrElse("40").asInstanceOf[String].toInt
      )
      self.reply(Mobile.asJSON(viewport._1,viewport._2,viewport._3,viewport._4))
  }
}

class JobActor(queue: JobQueue, blockMap:BlockMap ) extends Actor with Consumer {
  def endpointUri = "jetty:http://localhost:8888/jobs"

  def receive = {
    case msg: Message =>
      if( msg.body != null ) {
        import net.liftweb.json._
        implicit val formats = DefaultFormats
        var json = parse(msg.bodyAs[String])
        val x = (json \\ "x").extract[Int]
        val y = (json \\ "y").extract[Int]
        val z = (json \\ "z").extract[Int]
        val job = (json \\ "job").extract[String]
        blockMap.blockAt(new Coord(x,y,z)) match {
          case Some(block) => {
            job match {
              case "dig" => {
                val njob = new DigJob(block,queue)
                self.reply(njob.toJson)
              }
              case "build" => {
                val njob = new BuildJob(block,queue)
                self.reply(njob.toJson)
              }
            }
          }
          case None => ()
        }
      } else {
        self.reply(queue.toJson) //(viewport._1,viewport._2,viewport._3,viewport._4))
      }
  }
}

class BuildingTemplateActor extends Actor with Consumer {
  def endpointUri = "jetty:http://localhost:8888/templates"

  def receive = {
    case msg: Message =>
      // val coords = msg.headers(Set("startx","starty","width","height"))
      // val viewport = (
      //   coords.get("startx").getOrElse("0" ).asInstanceOf[String].toInt,
      //   coords.get("starty").getOrElse("0" ).asInstanceOf[String].toInt,
      //   coords.get("width" ).getOrElse("20").asInstanceOf[String].toInt,
      //   coords.get("height").getOrElse("40").asInstanceOf[String].toInt
      // )
      self.reply(BuildingTemplate.toJson)
  }
}
class BlockMapActor(blockMap:BlockMap) extends Actor with Consumer {
  def endpointUri = "jetty:http://localhost:8888/map"

  def receive = {
    case msg: Message =>
      if( msg.body != null ) {
        import net.liftweb.json._
        implicit val formats = DefaultFormats
        var json = parse(msg.bodyAs[String])
        val x = (json \\ "x").extract[Int]
        val y = (json \\ "y").extract[Int]
        val z = (json \\ "z").extract[Int]
        val selected = (json \\ "selected").extract[Boolean]
        val block = blockMap.blockAt(new Coord(x,y,z))
        block.get.selected = selected
        self.reply(block.get.toJson)
      } else {
        var coords = msg.headers(Set("startx","starty","width","height"))
        var viewport = (
          coords.get("startx").getOrElse("0" ).asInstanceOf[String].toInt,
          coords.get("starty").getOrElse("0" ).asInstanceOf[String].toInt,
          coords.get("width" ).getOrElse("20").asInstanceOf[String].toInt,
          coords.get("height").getOrElse("40").asInstanceOf[String].toInt
        )
        self.reply(blockMap.asJSON(viewport._1,viewport._2,viewport._3,viewport._4))
      }
  }
}

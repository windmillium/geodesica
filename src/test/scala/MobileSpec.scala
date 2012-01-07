import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

class MobileSpec extends Spec with ShouldMatchers {
  describe("updateJob"){
    it("should return None if no jobs"){
      val queue = new JobQueue
      val mob = new Mobile
      mob.queue = queue
      expect(None){
        mob.updateJob
      }
    }

    it("should return a job if one exists in the queue"){
      val queue = new JobQueue
      val mob = new Mobile
      mob.queue = queue
      val job = new FooJob(queue)
      expect(Some(job)){
        mob.updateJob
      }
    }
    
  }

  describe("unfullfilledObjects") {
    it("should return the requirement object if it is unfullfilled") {
      import collection.mutable.ListBuffer
      val mob = new Mobile(new MobileSpecies("test"))
      val ot = new ObjectTemplate("Rubble")
      val requirements = List(new ConsumableRequirement(ot))
      expect(mob.unfullfilledObjects(requirements)) {
        List(ot)
      }
    }

    it("shouldn't return anything if all requirements are fullfilled") {
      import collection.mutable.ListBuffer
      val mob = new Mobile(new MobileSpecies("test"))
      val ot = new ObjectTemplate("Rubble")
      mob.objects += ot.create
      val requirements = List(new ConsumableRequirement(ot))
      expect(mob.unfullfilledObjects(requirements)) {
        List()
      }
    }
  }
}

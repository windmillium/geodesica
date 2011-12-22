import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import world._

class MobileSpec extends Spec with ShouldMatchers {
  class FooJob(queue:JobQueue) extends Job(queue) with WithID[Job]
  describe("update") {
    it("should look for a job") {
      val queue = new JobQueue("mobileupdate")
      val mob = new Mobile(new MobileSpecies("test"))
      mob.queue = queue
      val job = new FooJob(queue)
      mob.update
      expect(mob.job) {
        Some(job)
      }
    }
  }

  describe("unfullfilledObjects") {
    it("should return the requirement object if it is unfullfilled") {
      import collection.mutable.ListBuffer
      val mob = new Mobile(new MobileSpecies("test"))
      val ot = new ObjectTemplate("Rubble")
      val requirements = List(new Requirement(ot))
      expect(mob.unfullfilledObjects(requirements)) {
        List(ot)
      }
    }

    it("shouldn't return anything if all requirements are fullfilled") {
      import collection.mutable.ListBuffer
      val mob = new Mobile(new MobileSpecies("test"))
      val ot = new ObjectTemplate("Rubble")
      mob.objects += ot.create
      val requirements = List(new Requirement(ot))
      expect(mob.unfullfilledObjects(requirements)) {
        List()
      }
    }
  }
}

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

object FooProfession extends Profession {
  def createJob(mobile:Mobile) = {
    Some(new FooJob(mobile.queue))
  }
}

class FinishedJob(val block:Block = new Block) extends Job(new JobQueue) {
  override def finished = true
  def finalTask(mobile:Mobile) = Some(new WaitTask(1))
}

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

    it("should return the current job"){
      val queue = new JobQueue
      val queue2 = new JobQueue
      val mob = new Mobile
      mob.queue = queue2
      val job = new FooJob(queue)
      mob.job = Some(job)
      expect(Some(job)){
        mob.updateJob
      }
    }

    it("should return none if current job is finished"){
      val mob = new Mobile
      mob.queue = new JobQueue
      mob.job = Some(new FinishedJob)
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

    it("should limit jobs by professions"){
      val queue = new JobQueue
      val mob = new Mobile
      mob.queue = queue
      mob.professions -= General
      val job = new FooJob(queue)
      expect(None){
        mob.updateJob
      }
    }

    it("should create a profession job"){
      val queue = new JobQueue
      val mob = new Mobile
      mob.queue = queue
      mob.professions -= General
      mob.professions += FooProfession
      assert(mob.updateJob.get.isInstanceOf[Job])      
    }
  }

  describe("updateTask"){
    val task = new WaitTask(1)
    class TaskJob(val block:Block = new Block) extends Job(new JobQueue) {
      def finalTask(mobile:Mobile) = {
        Some(task)
      }
    }

    it("should return the next task from job"){
      val mob = new Mobile
      mob.queue = new JobQueue
      mob.job = Some(new TaskJob)
      expect(Some(task)){
        mob.updateTask
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

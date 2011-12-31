import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

class JobQueueSpec extends Spec with ShouldMatchers {
  class FooJob(queue:JobQueue) extends Job(queue) with WithID[Job]
  describe("findJob") {
    it("should return the first open job") {
      val queue = new JobQueue("findjob")
      val mob = new Mobile(new MobileSpecies("test"))
      new FooJob(queue).owner = Some(mob)
      val job = new FooJob(queue)
      expect(Some(job)) {
        queue.findJob(mob.professions)
      }
    }
  }

  describe("openJobs") {
    it("should find all open jobs") {
      val queue = new JobQueue("openjobs")
      val size = queue.openJobs.size
      new FooJob(queue)
      new FooJob(queue)
      val ownedJob = new FooJob(queue).owner = Some(new Mobile(new MobileSpecies("test")))
      expect(queue.openJobs.contains(ownedJob)) {
        false
      }
    }
  }
}

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import world._

class JobQueueSpec extends Spec with ShouldMatchers {
  class FooJob(queue:JobQueue) extends Job(queue) with WithID[Job]
  describe("findJob") {
    it("should return the first open job") {
      val queue = new JobQueue("findjob")
      val mob = new Mobile
      new FooJob(queue).owner = Some(mob)
      val job = new FooJob(queue)
      expect(queue.findJob) {
        Some(job)
      }
    }
  }

  describe("openJobs") {
    it("should find all open jobs") {
      val queue = new JobQueue("openjobs")
      val size = queue.openJobs.size
      new FooJob(queue)
      new FooJob(queue)
      val ownedJob = new FooJob(queue).owner = Some(new Mobile)
      expect(queue.openJobs.contains(ownedJob)) {
        false
      }
    }
  }
}

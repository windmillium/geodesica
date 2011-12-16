import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import world._

class MobileSpec extends Spec with ShouldMatchers {
  class FooJob(queue:JobQueue) extends Job(queue) with WithID[Job]
  describe("update") {
    it("should look for a job") {
      val queue = new JobQueue("mobileupdate")
      val mob = new Mobile
      mob.queue = queue
      val job = new FooJob(queue)
      mob.update
      expect(mob.job) {
        Some(job)
      }
    }
  }
}

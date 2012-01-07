import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._

class FooJob(queue:JobQueue, val block:Block = new Block(new BlockMap,new Coord(0,0,0))) extends Job(queue) with WithID[Job]
class JobQueueSpec extends Spec with ShouldMatchers {
  describe("findJob") {
    it("should return the first open job") {
      val queue = new JobQueue
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
      val queue = new JobQueue
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

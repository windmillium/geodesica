import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import net.geodesica._
import TimeWrapper._

class TimeSpec extends Spec with ShouldMatchers {
  it("should convert to milliseconds") {
    expect(10 milliseconds) {
      10
    }
  }
  it("should convert to seconds") {
    expect(10 seconds) {
      10000
    }
  }
  it("should convert to minutes") {
    expect(10 minutes) {
      600000
    }
  }
  it("should convert to hours") {
    expect(10 hours) {
      36000000
    }
  }
  it("should convert to days") {
    expect(10 days) {
      864000000
    }
  }
}

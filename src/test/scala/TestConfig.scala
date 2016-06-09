
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import com.typesafe.config.ConfigFactory

class TestConfig extends FlatSpec {
  "Application properties" should "load" in {
    val config = ConfigFactory.load
    config.getBoolean("test.property") shouldBe true
    config.getString("source.build") shouldBe "../SIREC-BuildDB-CurrentAndTarget/source_build"
    config.getString("target.build") shouldBe "../SIREC-BuildDB-CurrentAndTarget/target_build"
  }
}

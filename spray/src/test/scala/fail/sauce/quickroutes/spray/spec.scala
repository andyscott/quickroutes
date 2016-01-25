package fail.sauce.quickroutes.spray

import org.scalatest._
import spray.testkit.ScalatestRouteTest
import spray.routing.HttpService
import spray.http.StatusCodes._

class FullTestKitExampleSpec extends FunSpec with Matchers with ScalatestRouteTest with HttpService {
  def actorRefFactory = system

  type DeviceId = String

  val simpleRoute =
    QuickRoutes().append {

      case get"/users" ⇒
        complete("get all users")

      case get"/users/$id" ⇒
        complete(s"get user $id")

      case get"/users/$id/devices/${ deviceId: DeviceId }" ⇒
        complete(s"get $id's device $deviceId")

      case post"/installations" ⇒
        complete("create a new installation")
    }

  describe("the service") {

    it("works") {
      Get("/ping") ~> simpleRoute ~> check {
        responseAs[String] should equal("PONG!")
      }
    }
  }
}

import uk.gov.hmrc.play.test.UnitSpec

import models._
import logic._

import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.FakeApplication

import org.scalatest._
import org.scalatest.concurrent._
import scala.concurrent._

class ControllerITSpec extends UnitSpec {
  "Calculator controller" should {
    "return list of summary allowances for each input" in {
      play.api.Play.start(FakeApplication())
      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate"));
      status(result.get) should not be 200
    }
  }
} 
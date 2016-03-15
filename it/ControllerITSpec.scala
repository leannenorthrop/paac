import uk.gov.hmrc.play.test.UnitSpec

import models._
import logic._

import play.api.Play
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.FakeApplication

import org.scalatest._
import org.scalatest.concurrent._
import scala.concurrent._
import play.api.libs.json._

class ControllerITSpec extends UnitSpec with BeforeAndAfterAll {
  val app = FakeApplication()

  override def beforeAll() {
    Play.start(app)
    super.beforeAll() // To be stackable, must call super.beforeEach
  }

  override def afterAll() {
    try {
      super.afterAll()
    } finally Play.stop()
  }

  "Calculator controller" should {
    "return error if no JSON supplied" in {
      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate"))
      status(result.get) should not be 200
    }

    "return list of summary allowances for each input" in {
      val contributions = Seq(Contribution(TaxPeriod(2013,3,1),TaxPeriod(2014,2,31),InputAmounts(definedBenefit=5000)))
      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate").withBody(Json.toJson(contributions)))
      val results = (contentAsJson(result.get) \ "results").as[List[TaxYearResults]]
      results(0).summaryResult shouldBe SummaryResult()
      status(result.get) shouldBe 200
    }
  }
} 
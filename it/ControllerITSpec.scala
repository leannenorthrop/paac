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
      val contribution0 = Contribution(TaxPeriod(2008, 4, 1), TaxPeriod(2009, 3, 31), InputAmounts(definedBenefit=5000))
      val contribution1 = Contribution(TaxPeriod(2009, 4, 1), TaxPeriod(2010, 3, 31), InputAmounts(definedBenefit=6000))
      val contribution2 = Contribution(TaxPeriod(2010, 4, 1), TaxPeriod(2011, 3, 31), InputAmounts(definedBenefit=7000))
      val contribution3 = Contribution(TaxPeriod(2011, 4, 1), TaxPeriod(2012, 3, 31), InputAmounts(definedBenefit=8000))
      val contribution4 = Contribution(TaxPeriod(2013, 4, 1), TaxPeriod(2014, 3, 31), InputAmounts(definedBenefit=9000))
      val contributions = Seq(contribution0, contribution1, contribution2, contribution3, contribution4)

      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate").withBody(Json.toJson(contributions)))

      val results = (contentAsJson(result.get) \ "results").as[List[TaxYearResults]]
      results.size shouldBe 5
      status(result.get) shouldBe 200
    }
  }
} 
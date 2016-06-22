//import uk.gov.hmrc.play.test.UnitSpec
//import models._
//import play.api.Play
//import play.api.mvc._
//import play.api.test.FakeRequest
//import play.api.test.Helpers._
//import play.api.test.FakeApplication
//import org.scalatest._
//import scala.concurrent._
//import play.api.libs.json._
//
//class ControllerITSpec extends UnitSpec with BeforeAndAfterAll {
//  val app = FakeApplication()
//
//  override def beforeAll() {
//    Play.start(app)
//    super.beforeAll() // To be stackable, must call super.beforeEach
//  }
//
//  override def afterAll() {
//    try {
//      super.afterAll()
//    } finally Play.stop()
//  }
//
//  "Calculator controller" should {
//    "return error if no JSON supplied" in {
//      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate"))
//      status(result.get) should not be 200
//    }
//
//    "return BAD REQUEST if tax year is less than earliest supported tax year" in {
//      val contribution0 = Contribution(1921, 5000)
//      val contributions = Seq(contribution0)
//      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate").withBody(Json.toJson(contributions)))
//      status(result.get) shouldBe 400
//      val results = contentAsJson(result.get)
//      (results \ "status") shouldBe JsNumber(400)
//      (results \ "message") shouldBe JsString("Invalid JSON request object.")
//    }
//
//    "return BAD REQUEST if tax year is greater than latest supported tax year" in {
//      val contribution0 = Contribution(4400, 5000)
//      val contributions = Seq(contribution0)
//      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate").withBody(Json.toJson(contributions)))
//      status(result.get) shouldBe 400
//      val results = contentAsJson(result.get)
//      (results \ "status") shouldBe JsNumber(400)
//      (results \ "message") shouldBe JsString("Unsupported tax year supplied, only tax years between 2008 and 2016 inclusive, are supported.")
//    }
//
//    "return list of summary allowances for each input" in {
//      //val contribution0 = Contribution(2006, 3000)
//      //val contribution1 = Contribution(2007, 4000)
//      val contribution2 = Contribution(2008, 5000)
//      val contribution3 = Contribution(2009, 6000)
//      val contribution4 = Contribution(2010, 7000)
//      val contribution5 = Contribution(2011, 8000)
//      val contribution6 = Contribution(2012, 9000)
//      val contribution7 = Contribution(2013, 10000)
//      val contribution8 = Contribution(2014, 11000)
//
//      //val contributions = Seq(contribution0, contribution1, contribution2, contribution3, contribution4, contribution5, contribution6, contribution7, contribution8)
//      val contributions = Seq(contribution2, contribution3, contribution4, contribution5, contribution6, contribution7, contribution8)
//
//      val result : Option[Future[Result]] = route(FakeRequest(POST, "/paac/calculate").withBody(Json.toJson(contributions)))
//
//      val results = (contentAsJson(result.get) \ "results").as[List[TaxYearResults]]
//      results.size shouldBe 7
//      status(result.get) shouldBe 200
//    }
//  }
//}
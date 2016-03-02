package controllers

import play.api.mvc.{ Action}
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.Future

/**
  * Created by peri on 02/03/16.
  */
object CalculatorController extends CalculatorController

  trait CalculatorController extends BaseController {

    def index() = Action.async {implicit request =>
       Future.successful(Ok("Hi"))
    }

    def results() = Action.async { implicit request =>
      Future.successful(Ok("Herld"))
    }
}
package controllers

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import javax.inject.Inject

import play.api.libs.iteratee.{Concurrent, Iteratee}
import play.api.libs.ws._
import play.api.mvc._

class Application @Inject()(ws: WSClient) extends Controller {
  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def wsSF = WebSocket.using[String] { request =>
    (Iteratee.ignore[String], Application.sfOutEnumerator)
  }
}

object Application {

  val (sfOutEnumerator, sfOutChannel) = Concurrent.broadcast[String]

  // Get and push a new script chunk every 500ms
  val f = new ScheduledThreadPoolExecutor(1).scheduleAtFixedRate(new Runnable {
    def run() = sfOutChannel.push(helpers.Markov.next)
  }, 0, 250, TimeUnit.MILLISECONDS)

}
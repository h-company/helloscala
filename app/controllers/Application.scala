package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import jp.t2v.lab.play20.auth._
import views.html
import play.api.libs.iteratee.{Concurrent, Enumerator, Iteratee}
import collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicReference

object Application extends Controller with LoginLogout with AuthConfigImpl with Auth {

  val channels = new AtomicReference(Set[Concurrent.Channel[String]]())

  val nameMap = Map()

  type LoginForm = Form[(String, String)]

  val loginForm: LoginForm = Form {
    tuple("email" -> email, "password" -> nonEmptyText)
  }

  def index = authorizedAction(models.NormalUser) { user =>
    _ => Ok(views.html.index(user.fullname))
  }

  def login = Action {
    Ok(views.html.login(loginForm))
  }

  def indexWebSocket = WebSocket.using[String] { request =>

    val (out, channel) = Concurrent.broadcast[String]
    var c: Set[Concurrent.Channel[String]] = null
    do {
      c = channels.get()
    } while (!channels.compareAndSet(c, c + channel))

  // Log events to the console
    val in = Iteratee.foreach[String]{message => {println(request.remoteAddress + ":" + message);channels.get.foreach(_.push(nameMap(request.remoteAddress) + ":" + message))}}.mapDone { _ =>
      var c: Set[Concurrent.Channel[String]] = null
      do {
        c = channels.get()
      } while (!channels.compareAndSet(c, c - channel))
    }

    (in, out)
  }
  //  def authenticate = Action {
  //    implicit request =>
  //      loginForm.bindFromRequest.fold(
  //      formWithErrors => BadRequest(html.login(formWithErrors)), {
  //        case (email, password) => models.User.authenticate(email, password) match {
  //          case None => Ok (views.html.login (loginForm.fill (email, "").withGlobalError("email or password is wrong.") ) )
  //          case Some (u) => gotoLoginSucceeded (u.id)
  //        }
  //      }
  //      )
  //  }

  def authenticate = Action {
    implicit request =>
      loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)), {
        case (email, password) =>
          models.User.authenticate(email, password)
            .map(_.id)
            .map(gotoLoginSucceeded)
            .getOrElse(
            BadRequest(html.login(loginForm.fill((email, "")).withGlobalError("emailもしくはパスワードが間違っています。"))))
      }
      )
  }

  def logout = Action {implicit request =>
    gotoLogoutSucceeded
  }
}
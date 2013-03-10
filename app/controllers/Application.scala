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

  val channels = TrieMap[String, Concurrent.Channel[String]]()

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

  val KickAll = """^/kick all""".r
  val Kick = """^/kick (.*)""".r
  val Who = """^/who$""".r
  val Direct = """^@(.*?) (.*)""".r

  def indexWebSocket = WebSocket.using[String] { request =>

    val name = nameMap(request.remoteAddress)

    val (out, channel) = Concurrent.broadcast[String]
    channels.putIfAbsent(name, channel)
    printAll(name + "たんインしたお")

  // Log events to the console
    val in = Iteratee.foreach[String] {
      case KickAll() => {printAll(name + "にキックされました");channels.values.foreach(_.eofAndEnd())}
      case Kick(target) => {(channels remove target).foreach{channel => print(name + "にキックされました", channel);channel.eofAndEnd()}}
      case Who() => print(channels.keys.toString, channels(name))
      case Direct(target, msg) => print(name + ":@" + target + " " + msg, channels(target))
      case message => printAll(name + ":" + message)
    }.mapDone { _ =>
      channels -= name
      printAll(name + " はいなくなった")
    }

    (in, out)
  }
  private def print(msg: String, channel: Concurrent.Channel[String]) {
    println(msg)
    channel.push(msg)
  }

  private def printAll(msg: String) {
    println(msg)
    channels.values.foreach(_.push(msg))
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

  def chat = Action {
    Ok(views.html.chat(""));
  }
}
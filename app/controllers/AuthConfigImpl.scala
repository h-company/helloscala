package controllers

import jp.t2v.lab.play20.auth.AuthConfig
import play.api.mvc._
import play.api.mvc.Results._

/**
 * Created with IntelliJ IDEA.
 * User: W.H
 * Date: 13/01/26
 * Time: 19:09
 * To change this template use File | Settings | File Templates.
 */
trait AuthConfigImpl extends AuthConfig {
  type Id = Long
  type User = models.User
  type Authority = models.Role
  val idManifest: ClassManifest[Id] = classManifest[Id]
  val sessionTimeoutInSeconds: Int = 3600

  def resolveUser(id: Id): Option[User] = models.User.findById(id)

  def loginSucceeded(request: RequestHeader): Result = Redirect(routes.Application.index)

  def logoutSucceeded(request: RequestHeader): Result = Redirect(routes.Application.login)

  def authenticationFailed(request: RequestHeader): Result = Redirect(routes.Application.login)

  def authorizationFailed(request: RequestHeader): Result = Forbidden("no permission")

  def authorize(user: User, authority: Authority): Boolean =
    (user.isAdmin, authority) match {
      case (true, _) => true
      case (false, models.NormalUser) => true
      case _ => false
    }

}

package draw.server.user

import java.nio.charset.StandardCharsets

import zio.http.{Body, Form, Header, Request, URL, ZClient}
import zio.schema.codec.JsonCodec.schemaBasedBinaryCodec
import zio.schema.{DeriveSchema, Schema}
import zio.{IO, ZIO, ZLayer}

import draw.server.ServerConfig

trait Github {
  import Github._

  /** Exchanges the given code for an OAuth token */
  def activate(code: String): IO[GithubError, String]

  /** Returns information about the OAuth token */
  def getToken(token: String): IO[GithubError, Token]
}

object Github {
  case class GithubError(message: String)

  case class Token(
    user: TokenUser
  )
  object Token {
    implicit val schema: Schema[Token] = DeriveSchema.gen
  }
  case class TokenUser(
    login: String,
    id: Long
  )
  object TokenUser {
    implicit val schema: Schema[TokenUser] = DeriveSchema.gen
  }

  val live = ZLayer.fromZIO(make)

  def make = for {
    config <- ZIO.service[ServerConfig]
    client <- ZIO.service[zio.http.Client]
    scope <- ZIO.scope
  } yield new Github {
    val layer = ZLayer.succeed(client) ++ ZLayer.succeed(scope)

    case class OauthTokenResponse(access_token: Option[String], error: Option[String])
    object OauthTokenResponse {
      implicit val schema: Schema[OauthTokenResponse]= DeriveSchema.gen
    }

    def activate(code: String): IO[GithubError, String] = (for {
      resp <- ZClient.request(Request.post(
        URL.decode("https://github.com/login/oauth/access_token").toOption.get,
        Body.fromURLEncodedForm(Form.fromStrings("code" -> code), StandardCharsets.UTF_8))
        .addHeader("Accept", "application/json")
        .addHeader(Header.Authorization.Basic(config.github.clientId, config.github.secret))
      ).mapError(toError)
      // We always get 200 OK, also on invalid codes...
      res <- resp.body.to[OauthTokenResponse].mapError(toError).flatMap {
        case OauthTokenResponse(Some(token), _) => ZIO.succeed(token)
        case OauthTokenResponse(_, Some(error)) => ZIO.fail(GithubError(error))
        case _ => ZIO.fail(GithubError("Unexpected github response"))
      }
    } yield {
      res
    }).provide(layer)

    def getToken(token: String): IO[GithubError, Token]  = (for {
      resp <- ZClient.request(Request.post(
        URL.decode(s"https://api.github.com/applications/${config.github.clientId}/token").toOption.get,
        Body.from(OauthTokenResponse(Some(token), None)))
        .addHeader("Accept", "application/json")
        .addHeader(Header.Authorization.Basic(config.github.clientId, config.github.secret))
      )
      //r <- resp.body.asString
      //_ = println(r)
      res <- resp.body.to[Token]
    } yield {
      res
    }).provide(layer).mapError(toError)
  }

  def toError(x: Throwable) = {
    x.printStackTrace()
    GithubError(x.toString)
  }
}

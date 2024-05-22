package draw.server.user

import java.util.UUID

import zio.{Clock, IO, ZIO, ZLayer}

import draw.server.ServerConfig
import palanga.zio.cassandra.ZStatement.StringOps
import palanga.zio.cassandra.{CassandraException, ZCqlSession}

object CassandraUsers {
  import Users._

  val live = ZLayer.fromZIO(make)

  val make = for {
    session <- ZIO.service[ZCqlSession]
    github <- ZIO.service[Github]
    config <- ZIO.service[ServerConfig]
    layer = ZLayer.succeed(session)
  } yield new Users {
    override def activateGithub(sessionId: UUID, code: String): IO[UserError, User] = (for {
      now <- Clock.instant
      tokenId <- github.activate(code)
      token <- github.getToken(tokenId)
      existing <- ZCqlSession.executeHeadOption(
        "SELECT userId from githubUser WHERE githubId = ?"
          .toStatement
          .bind(token.user.id)
          .decodeAttempt(_.getUuid("userId"))
      )
      userId = existing.getOrElse(UUID.randomUUID())
      user = User(userId, token.user.login)
      _ <- ZCqlSession.untyped.execute(
        "INSERT INTO userSessions (sessionId, userId, expiration) VALUES (?,?,?) USING TTL ?"
          .toStatement
          .bind(sessionId, userId, now.plus(config.github.ttl), config.github.ttl.toSeconds().toInt)
      )
      _ <- ZCqlSession.untyped.execute(
        "INSERT INTO users (userid, nickname) VALUES (?, ?) IF NOT EXISTS"
          .toStatement
          .bind(userId, user.nickname)
      )
      _ <- ZCqlSession.untyped.execute(
        "INSERT INTO githubUser (githubId, userId) VALUES (?, ?)"
          .toStatement
          .bind(token.user.id, userId)
      )
    } yield user).mapError(toError).provide(layer)

    override def authorize(sessionId: UUID): IO[UserError, User] = (for {
      now <- Clock.instant
      id <- ZCqlSession.executeHeadOrFail(
        "SELECT userId FROM userSessions WHERE sessionId = ? AND expiration >= ? LIMIT 1 ALLOW FILTERING"
          .toStatement
          .bind(sessionId, now)
          .decodeAttempt(_.getUuid("userId"))
      )
      nickname <- ZCqlSession.executeHeadOrFail(
        "SELECT nickname FROM users WHERE userId = ? LIMIT 1"
          .toStatement
          .bind(id)
          .decodeAttempt(_.getString("nickname"))
      )
    } yield {
      User(id, nickname)
    }).mapError(toError).provide(layer)

  }

  private def toError(x: CassandraException | Github.GithubError) = UserError(x.toString())

}

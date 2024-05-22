package draw.server.user

import java.util.UUID

import zio.schema.{DeriveSchema, Schema}
import zio.{IO, Ref, ZIO, ZLayer}

import Users._

trait Users {
  def activateGithub(sessionId: UUID, code: String): IO[UserError, User]
  def authorize(sessionId: UUID): IO[UserError, User]
}

object Users {
  case class User(id: UUID, nickname: String)

  object User {
    implicit val schema: Schema[User]= DeriveSchema.gen[User]
  }

  case class UserError(message: String)
  object UserError {
   implicit val schema: Schema[UserError] = DeriveSchema.gen[UserError]
  }

  val inMemory = ZLayer.fromZIO {
    case class State(users: Map[UUID, User] = Map.empty) {
      def activateGithub(sessionId: UUID, code: String): (User, State) = {
        val existing = users.get(sessionId)
        existing.map((_, this)).getOrElse {
          val user = User(sessionId, s"user-${sessionId}")
          (user, copy(users = users + (sessionId -> user)))
        }
      }

      def authorize(sessionId: UUID): Option[User] = users.get(sessionId)
    }

    for {
      state <- Ref.make(State())
    } yield new Users {
      def activateGithub(sessionId: UUID, code: String) = state.modify(_.activateGithub(sessionId, code))
      def authorize(sessionId: UUID) = state.get.map(_.authorize(sessionId)).flatMap(ZIO.fromOption).mapError(_ => UserError("Unknown session ID"))
    }
  }
}

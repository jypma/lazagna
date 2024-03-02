package draw.server

import java.security.MessageDigest

import zio.schema.{DeriveSchema, Schema}
import zio.{IO, Ref, ZIO, ZLayer}

import Users._

trait Users {
  def login(userName: String, password: String): IO[UserError, User]
  def authenticate(token: String): IO[UserError, User]
}

object Users {
  case class User(id: Long, name: String, token: String)
  object User {
    implicit val schema: Schema[User]= DeriveSchema.gen[User]
  }

  case class UserError(message: String)
  object UserError {
   implicit val schema: Schema[UserError] = DeriveSchema.gen[UserError]
  }

  val inMemory = ZLayer.fromZIO {
    def md5(s: String): String = {
      val md = MessageDigest.getInstance("MD5")
      md.update(s.getBytes())
      val sb = new StringBuilder
      for (b <- md.digest()) {
        sb.append(String.format("%02x", Byte.box(b)))
      }
      sb.toString
    }

    case class State(tokens: Map[String, String] = Map.empty, users: Map[String,User] = Map.empty) {
      def login(userName: String): (User, State) = {
        // TODO: Actually register and login users with actual passwords :)
        users.get(userName) match {
          case Some(user) =>
            (user, this)
          case None =>
            val token = md5(userName)
            val user = User(users.size, userName, token)
            (user, copy(tokens = tokens + (token -> userName), users = users + (userName -> user)))
        }
      }

      def authenticate(token: String): Option[User] = {
        tokens.get(token).map(users)
      }
    }

    for {
      state <- Ref.make(State())
    } yield (new Users {
      override def login(userName: String, password: String) = {
        state.modify(_.login(userName))
      }
      override def authenticate(token: String) = state.get.flatMap(_.authenticate(token) match {
        case Some(user) => ZIO.succeed(user)
        case None => ZIO.fail(UserError("Invalid token"))
      })
    }: Users)
  }
}

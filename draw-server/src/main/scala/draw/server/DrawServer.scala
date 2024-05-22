package draw.server

import java.net.InetAddress
import java.util.UUID

import scala.util.Try

import zio._
import zio.http.ChannelEvent.Read
import zio.http.Header.{AccessControlAllowMethods, AccessControlAllowOrigin, Origin}
import zio.http.Middleware.{CorsConfig, cors}
import zio.http._
import zio.schema.codec.JsonCodec.schemaBasedBinaryCodec

import draw.data.drawcommand.DrawCommand
import draw.server.drawing.Drawings.DrawingError
import draw.server.drawing.{CassandraDrawings, Drawing, Drawings}
import draw.server.user.Users.User
import draw.server.user.{CassandraUsers, Github, Users}
import palanga.zio.cassandra.CassandraException.SessionOpenException
import palanga.zio.cassandra.{ZCqlSession, session}

object DrawServer extends ZIOAppDefault {
  // Create CORS configuration
  val corsConfig: CorsConfig =
    CorsConfig(
      allowedOrigin = {
        case origin @ Origin.Value(_, host, _) if host == "localhost" =>
          println("Found localhost!")
          Some(AccessControlAllowOrigin.Specific(origin))
        case _                                                  =>
          println("Found other!")
          None
      },
      allowedMethods = AccessControlAllowMethods(Method.PUT, Method.DELETE),
    )

  private def drawingSocket(userId: UUID, drawing: Drawing, afterSequenceNr: Long): WebSocketApp[Any] =
    Handler.webSocket { channel =>
      ZIO.scoped {
        for {
          _ <- drawing.eventsAfter(afterSequenceNr).mapZIO { event =>
            val bytes = event.toByteArray
            channel.send(Read(WebSocketFrame.Binary(Chunk.fromArray(bytes))))
          }.runDrain.forkScoped
          res <- channel.receiveAll {
            case Read(WebSocketFrame.Ping) =>
              channel.send(Read(WebSocketFrame.Pong))
            case Read(WebSocketFrame.Binary(commandBytes)) =>
              val command = DrawCommand.parseFrom(commandBytes.toArray)
              drawing.perform(command).mapError { e =>
                println(e.toString)
                // TODO: Find out why WebSocket is modeled to only allow Throwable as error
                IllegalStateException(e.toString)
              }
            case _  =>
              ZIO.unit
          }
        } yield res
      }
    }

  val cassandraSession = ZLayer.fromZIO(for {
    config <- ZIO.service[ServerConfig]
    cfg = config.cassandra
    res <- session.auto.open(
      cfg.hostname,
      cfg.port,
      cfg.keyspace,
    )
  } yield res)

  def getSession(request: Request): IO[Users.UserError, UUID] = ZIO.fromOption {
    request.cookie("sessionId")
      .flatMap(c => Try(UUID.fromString(c.content)).toOption)
  }.mapError(_ => Users.UserError("TODO state"))

  def requireUser: HandlerAspect[ServerConfig & Users, User] =
    HandlerAspect.interceptIncomingHandler(Handler.fromFunctionZIO[Request] { request =>
      for {
        config <- ZIO.service[ServerConfig]
        users <- ZIO.service[Users]
        loginLinks = Seq(
          ("Login with Github", s"https://github.com/login/oauth/authorize?client_id=${config.github.clientId}")
        )
        // TODO: Case class and schema for these
        loginJson = loginLinks.map { t => s"""{"name":"${t._1}","link":"${t._2}"}"""}.mkString(",")
        unauthorized = (msg: String) => Response.json(s"""{"message":"$msg","login":[${loginJson}]}""").copy(status = Status.Unauthorized)
        sessionId <- ZIO.fromOption(request.cookie("sessionId").flatMap(c => Try(UUID.fromString(c.content)).toOption))
          .orElseFail(unauthorized("Missing sessionId cookie"))
        user <- users.authorize(sessionId)
          .mapError { e =>
            println(e)
            unauthorized("Invalid sessionId cookie")
          }
      } yield (request, user)
    })


  val app = for {
    users <- ZIO.service[Users]
    drawings <- ZIO.service[Drawings]
    config <- ZIO.service[ServerConfig]
  } yield Routes(
    Method.GET / "user" / "activate" -> handler { (request: Request) =>
      val sessionId = UUID.randomUUID() // FIXME time-based UUID
      for {
        code <- request.url.queryParamToZIO[String]("code")
        state <- request.url.queryParamToZIO[String]("state")
        // FIXME: Use and verify state (two states, rotating every 15 minutes)
        user <- users.activateGithub(sessionId, code)
      } yield Response(body = Body.from(user)).addCookie(Cookie.Response("sessionId", sessionId.toString,
        isSecure = true, isHttpOnly = true, maxAge = Some(config.github.ttl), sameSite = Some(Cookie.SameSite.Strict),
        path = Some(Path("/")), domain = Some("localhost")
      ))
    },
    Method.GET / "user" -> requireUser -> handler { (user: User, request: Request) =>
       Response(body = Body.from(user))
    },
    Method.HEAD / "drawings" / uuid("drawing") -> requireUser -> Handler.fromFunctionZIO[(UUID, User, Request)] { (drawing, user, request) =>
      for {
        drawing <- drawings.getDrawing(drawing)
        version <- drawing.version
      } yield Response.ok.addHeader(Header.ETag.Strong(version.toString))
    },
    Method.GET / "drawings" / uuid("drawing") / "socket" -> requireUser -> handler { (drawing: UUID, user: User, request: Request) =>
      val after = request.url.queryParam("afterSequenceNr").map(_.toLong).getOrElse(-1L)
      for {
        drawing <- drawings.getDrawing(drawing)
        res <- drawingSocket(user.id, drawing, after).toResponse
      } yield res
    },
    Method.GET / "drawings" -> requireUser -> handler { (user: User, request: Request) =>
      for {
        list <- drawings.list.runCollect
      } yield Response(body = Body.from(list))
    },
  ).handleError {
    case Users.UserError(message) =>
      val loginLinks = Seq(
        ("Login with Github", s"https://github.com/login/oauth/authorize?client_id=${config.github.clientId}")
      )
      val loginJson = loginLinks.map { t => s"""{"name":"${t._1}","link":"${t._2}"}"""}.mkString(",")

      Response.json(s"""{"message":"$message","login":[${loginJson}]}""").copy(status = Status.Unauthorized)
    case other => Response.badRequest(other.toString)
  }.toHttpApp @@ cors(corsConfig)

  override val run = app.flatMap(Server.serve).provideSome[Scope](
    ZLayer.succeed(Server.Config.default.binding(InetAddress.getByName("0.0.0.0"), 8080)),
    Server.live,
    CassandraUsers.live,
    ZLayer.fromZIO(CassandraDrawings.make),
    cassandraSession,
    ServerConfig.live,
    Github.live,
    zio.http.Client.default
  )
}

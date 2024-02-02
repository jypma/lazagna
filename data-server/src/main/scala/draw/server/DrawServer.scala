package draw.server

import java.net.InetAddress

import zio._
import zio.http.ChannelEvent.Read
import zio.http.Header.{AccessControlAllowMethods, AccessControlAllowOrigin, Origin}
import zio.http.Middleware.{CorsConfig, cors}
import zio.http._
import zio.http.codec.HttpCodec.query
import zio.http.codec.PathCodec.string
import zio.http.endpoint.Endpoint

import draw.data.drawcommand.DrawCommand
import draw.server.Users.User

object DrawServer extends ZIOAppDefault {
  // Create CORS configuration
  val config: CorsConfig =
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

  private def drawingSocket(userId: Long, drawing: Drawing, afterSequenceNr: Long): WebSocketApp[Any] =
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

  val app = for {
    users <- ZIO.service[Users]
    drawings <- ZIO.service[Drawings]
  } yield Routes(
    Endpoint(Method.POST / "users" / string("username") / "login")
      .outError[Users.UserError](Status.BadRequest)
      .query(query("password")) // TODO: Nicer error messages from zio-http on missing query param?
      .out[User](MediaType.application.json)
      .implement(Handler.fromFunctionZIO((username: String, password: String) =>
          users.login(username, password)
      )),
    Method.HEAD / "drawings" / string("drawing") -> handler { (drawing: String, request: Request) =>
      for {
        token <- request.url.queryParams.getAsZIO[String]("token")
        user <- users.authenticate(token)
        drawing <- drawings.getDrawing(drawing)
        version <- drawing.version
      } yield Response.ok.addHeader(Header.ETag.Strong(version.toString))
    },
    Method.GET / "drawings" / string("drawing") / "socket" -> handler { (drawing: String, request: Request) =>
      for {
        token <- request.url.queryParams.getAsZIO[String]("token")
        after = request.url.queryParams.get("afterSequenceNr").map(_.toLong).getOrElse(-1L)
        user <- users.authenticate(token)
        drawing <- drawings.getDrawing(drawing)
        res <- drawingSocket(user.id, drawing, after).toResponse
      } yield res
    }
  ).handleError {
    case Users.UserError(message) => Response.unauthorized(message)
    case other => Response.badRequest(other.toString)
  }.toHttpApp @@ cors(config)

  override val run = app.flatMap(Server.serve).provide(
    ZLayer.succeed(Server.Config.default.binding(InetAddress.getByName("0.0.0.0"), 8080)),
    Server.live,
    Users.inMemory,
    Drawings.inMemory
  )
}

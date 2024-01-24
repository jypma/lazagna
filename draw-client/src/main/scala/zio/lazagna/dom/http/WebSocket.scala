package zio.lazagna.dom.http

import org.scalajs.dom
import zio.ZIO
import zio.stream.ZStream
import zio.Chunk
import zio.Scope
import zio.Promise
import zio.IO
import WebSocket.WebSocketError
import scala.scalajs.js.typedarray.Int8Array
import scalajs.js.typedarray.AB2TA

trait WebSocket {
  def send(text: String): IO[WebSocketError, Unit]
  def send(bytes: Array[Byte]): IO[WebSocketError, Unit]
}

object WebSocket {
  sealed trait WebSocketError
  case class SocketFailed(message: String) extends WebSocketError

  /** Connects and handles the websocket, forking to a background fiber. */
  def handle(url: String)(onMessage: dom.MessageEvent => ZIO[Any, Nothing, Any]): ZIO[Scope, Nothing, WebSocket] = for {
    socket <- Promise.make[Nothing, WebSocket]
    _ <- ZStream.asyncScoped[Any, WebSocketError, Any] { cb =>
      for {
        domSocket <- ZIO.acquireRelease {
          ZIO.async[Any, WebSocketError, dom.WebSocket] { openedCB =>
            dom.console.log(url)
            val socket = new dom.WebSocket(url)
            socket.binaryType = "arraybuffer"
            socket.onopen = { event =>
              dom.console.log("open")
              openedCB(ZIO.succeed(socket))
            }
            socket.onmessage = { event =>
              //dom.console.log("message")
              cb(onMessage(event).map {
                //dom.console.log("  handled.")
                Chunk(_)
              })
            }
            socket.onerror = { event =>
              dom.console.log("error")
              val failed = SocketFailed(event.toString)
              openedCB(ZIO.fail(failed))
              cb(ZIO.fail(Some(failed)))
            }
            dom.console.log(socket)
          }
        } { socket =>
          ZIO.succeed {
            dom.console.log("close")
            socket.close()
          }
        }
        _ <- socket.complete(ZIO.succeed(new WebSocket {
          override def send(text:String) = {
            if (domSocket.readyState == 1)
              ZIO.succeed(domSocket.send(text))
            else
              ZIO.fail(SocketFailed(s"Socket is not ready to send data: ${domSocket.readyState}"))
          }

          override def send(bytes: Array[Byte]) = {
            if (domSocket.readyState == 1)
              ZIO.succeed(domSocket.send(bytes.toTypedArray.buffer))
            else
              ZIO.fail(SocketFailed(s"Socket is not ready to send data: ${domSocket.readyState}"))
          }

        }))
      } yield ()
    }.runDrain.forkScoped
    ws <- socket.await
  } yield ws

  /*
  def connect(url: String)(promise: Promise[Nothing, WebSocket]): ZStream[Scope, WebSocketError, dom.MessageEvent] = {
    ZStream.asyncScoped[Any, WebSocketError, dom.MessageEvent] { cb =>
      for {
        domSocket <- ZIO.acquireRelease {
          ZIO.async[Any, WebSocketError, dom.WebSocket] { openedCB =>
            val socket = new dom.WebSocket(url)
            socket.binaryType = "arraybuffer"
            socket.onopen = { event =>
              openedCB(ZIO.succeed(socket))
            }
            socket.onmessage = { event =>
              cb(ZIO.succeed(Chunk(event)))
            }
            socket.onerror = { event =>
              val failed = SocketFailed(event.toString)
              openedCB(ZIO.fail(failed))
              cb(ZIO.fail(Some(failed)))
            }
          }
        } { socket =>
          ZIO.succeed {
            socket.close()
          }
        }

        webSocket = new WebSocket {
          override def send(text:String) = {
            if (domSocket.readyState == 1)
              ZIO.succeed(domSocket.send(text))
            else
              ZIO.fail(SocketFailed(s"Socket is not ready to send data: ${domSocket.readyState}"))
          }
        }

        _ <- promise.complete(ZIO.succeed(webSocket))
      } yield ()
    }
  }*/
}

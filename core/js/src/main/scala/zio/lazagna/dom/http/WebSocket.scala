package zio.lazagna.dom.http

import zio.stream.ZStream
import zio.{Chunk, IO, Promise, Scope, ZIO}

import org.scalajs.dom

import WebSocket.WebSocketError
import scalajs.js.typedarray._

trait WebSocket {
  def send(text: String): IO[WebSocketError, Unit]
  def send(bytes: Array[Byte]): IO[WebSocketError, Unit]
}

object WebSocket {
  sealed trait WebSocketError
  case class SocketFailed(message: String) extends WebSocketError

  /** Connects and handles the websocket, forking to a background fiber. */
  def handle(url: String)(onMessage: dom.MessageEvent => ZIO[Any, Nothing, Any], onClose: => ZIO[Any, Nothing, Any] = ZIO.unit): ZIO[Scope, Nothing, WebSocket] = for {
    socket <- Promise.make[Nothing, WebSocket]
    _ <- ZStream.asyncScoped[Any, WebSocketError, Any] { cb =>
      for {
        domSocket <- ZIO.acquireRelease {
          ZIO.async[Any, WebSocketError, dom.WebSocket] { openedCB =>
            dom.console.log(url)
            val socket = new dom.WebSocket(url)
            socket.binaryType = "arraybuffer"
            socket.onopen = { event =>
              openedCB(ZIO.succeed(socket))
            }
            socket.onclose = { event =>
              cb(onClose *> ZIO.fail(None))
            }
            socket.onmessage = { event =>
              cb(onMessage(event).map {
                Chunk(_)
              })
            }
            socket.onerror = { event =>
              dom.console.log("Error!")
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
}

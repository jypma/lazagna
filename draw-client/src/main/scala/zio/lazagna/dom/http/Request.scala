package zio.lazagna.dom.http

import scala.scalajs.js
import zio.IO
import org.scalajs.dom
import zio.Promise
import zio.ZIO

object Request {
  case class ResponseHandler[T] private[http] (responseType: String, handle: js.Any => IO[RequestError, T])
  val AsString = ResponseHandler[String]("text", res => ZIO.succeed(res.toString))
  val AsDynamicJSON = ResponseHandler[js.Dynamic]("json", res => ZIO.succeed(res.asInstanceOf[js.Dynamic]))

  private def request[T](method: String, url: String, handler: ResponseHandler[T]): IO[RequestError, T] = {
    dom.console.log(s"${method} ${url}")
    ZIO.async[Any, RequestError, T] { cb =>
      val request = new dom.XMLHttpRequest
      request.responseType = handler.responseType
      // TODO: set Accept header using setRequestHeader, and allow ResponseHandler to say what
      request.onload = { event =>
        val res = request.response
        if (res == null) {
          cb(ZIO.fail(RequestFailed(request.status)))
        } else {
          cb(handler.handle(res))
        }
      }
      request.ontimeout = { event =>
        cb(ZIO.fail(RequestTimedOut))
      }
      request.onerror = { event =>
        cb(ZIO.fail(RequestFailed(request.status)))
      }
      request.open(method, url, true)
      request.send()
    }
  }

  def GET[T](handler: ResponseHandler[T], url: String): IO[RequestError, T] = request("GET", url, handler)
  def POST[T](handler: ResponseHandler[T], url: String): IO[RequestError, T] = request("POST", url, handler)

  sealed trait RequestError
  case class RequestFailed(statusCode: Int) extends RequestError
  case object RequestTimedOut extends RequestError
}

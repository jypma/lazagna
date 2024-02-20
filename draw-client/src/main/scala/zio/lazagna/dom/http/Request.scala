package zio.lazagna.dom.http

import scala.scalajs.js

import zio.{IO, ZIO}

import org.scalajs.dom

object Request {
  case class Response(private val request: dom.XMLHttpRequest) {
    def header(name: String): Option[String] = Option(request.getResponseHeader(name))
  }

  case class ResponseHandler[T] private[http] (responseType: String, handle: dom.XMLHttpRequest => IO[RequestError, T])
  val AsString = ResponseHandler[String]("text", r => ZIO.succeed(r.response.toString))
  def JSONAs[T <: js.Any] = ResponseHandler[T]("json", r => ZIO.succeed(r.response.asInstanceOf[T]))
  val AsDynamicJSON = JSONAs[js.Dynamic]
  val AsResponse = ResponseHandler[Response]("", r => ZIO.succeed(Response(r)))

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
          cb(handler.handle(request))
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
  def HEAD[T](handler: ResponseHandler[T], url: String): IO[RequestError, T] = request("HEAD", url, handler)
  def HEAD(url: String): IO[RequestError, Response] = HEAD(AsResponse, url)

  sealed trait RequestError
  case class RequestFailed(statusCode: Int) extends RequestError
  case object RequestTimedOut extends RequestError
}

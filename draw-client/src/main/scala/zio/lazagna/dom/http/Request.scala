package zio.lazagna.dom.http

import scala.scalajs.js

import zio.{IO, ZIO, UIO}

import org.scalajs.dom
import org.scalajs.dom.FileReader
import scala.scalajs.js.typedarray.ArrayBuffer

object Request {
  case class Response(private val request: dom.XMLHttpRequest) {
    def header(name: String): Option[String] = Option(request.getResponseHeader(name))
  }

  case class ResponseHandler[T] private[http] (responseType: String, handle: dom.XMLHttpRequest => IO[RequestError, T]) {
    def map[U](f: T => U) = copy(handle = handle.andThen(_.map(f)))
    def flatMap[U](f: T => UIO[U]) = copy(handle = handle.andThen(_.flatMap(f)))
  }
  val AsString = ResponseHandler[String]("text", r => ZIO.succeed(r.response.toString))
  val AsBlob = ResponseHandler[dom.Blob]("blob", { r => ZIO.succeed(r.response.asInstanceOf[dom.Blob])})
  val AsArrayBuffer = ResponseHandler[ArrayBuffer]("arraybuffer", { r => ZIO.succeed(r.response.asInstanceOf[ArrayBuffer])})
  val AsBase64: ResponseHandler[String] = AsArrayBuffer.flatMap { buf =>
    ZIO.async { cb =>
      val blob = new dom.Blob(js.Array(buf))
      val reader = new dom.FileReader
      println("Reading...")
      reader.onload = { event =>
        val res = reader.result.asInstanceOf[String]
        println(res)
        val pos = res.indexOf(",")
        cb(ZIO.succeed(res.substring(pos + 1)))
      }
      reader.onerror = { event =>
        dom.console.log(event)
      }
      reader.readAsDataURL(blob)
    }
  }
  def JSONAs[T <: js.Any] = ResponseHandler[T]("json", r => ZIO.succeed(r.response.asInstanceOf[T]))
  val AsDynamicJSON: ResponseHandler[js.Dynamic] = JSONAs[js.Dynamic]
  val AsResponse = ResponseHandler[Response]("", r => ZIO.succeed(Response(r)))
  val AsDocument = ResponseHandler[dom.Document]("document", r => ZIO.succeed(r.response.asInstanceOf[dom.Document]))

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

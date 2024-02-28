package draw.client

import scala.scalajs.js

import zio.lazagna.dom.http.Request
import zio.lazagna.dom.http.Request._
import zio.{UIO, ZIO}

trait SymbolIndex {
  def lookup(text: String): UIO[SymbolIndex.Result]
}

object SymbolIndex {
  case class Result(completions: Seq[String], symbols: Seq[SymbolRef])

  private def toSymbolRef(o: js.Dynamic) = SymbolRef(SymbolCategory(o.category.asInstanceOf[String]), o.icon.asInstanceOf[String])

  def make = for {
    data <- Request.GET(JSONAs[js.Object], "/symbol-index.json")
  } yield new SymbolIndex {
    def lookup(text: String) = ZIO.succeed {
      data.asInstanceOf[js.Dynamic].selectDynamic(text) match {
        case _ if text == "" =>
          Result(js.Object.keys(data).toSeq.sorted, Seq.empty)
        case v if js.isUndefined(v) =>
          val suggestions = js.Object.keys(data).filter(_.startsWith(text)).toSeq.sorted
          Result(suggestions, Seq.empty)
        case exactMatch =>
          val suggestions = js.Object.keys(data).filter(_.startsWith(text)).toSeq.sorted
          val matches = exactMatch.asInstanceOf[js.Array[js.Dynamic]]
          Result(suggestions, matches.toSeq.map(toSymbolRef))
      }
    }
  }
}

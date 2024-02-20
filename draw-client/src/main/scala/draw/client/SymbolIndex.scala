package draw.client

import org.scalajs.dom
import zio.lazagna.dom.http.Request
import zio.lazagna.dom.http.Request._
import scala.scalajs.js
import zio.UIO
import zio.ZIO

trait SymbolIndex {
  def lookup(text: String): UIO[SymbolIndex.Result]
}

object SymbolIndex {
  case class Result(completions: Seq[String], symbols: Seq[SymbolRef])

  private def toSymbolRef(o: js.Dynamic) = SymbolRef(o.category.asInstanceOf[String], o.icon.asInstanceOf[String])

  def make = for {
    data <- Request.GET(JSONAs[js.Object], "/symbol-index.json")
  } yield new SymbolIndex {
    def lookup(text: String) = ZIO.succeed {
      dom.console.log(data)
      try {
      data.asInstanceOf[js.Dynamic].selectDynamic(text) match {
        case _ if text == "" =>
          println("empty")
          Result(js.Object.keys(data).toSeq.sorted, Seq.empty)
        case v if js.isUndefined(v) =>
          val suggestions = js.Object.keys(data).filter(_.startsWith(text)).toSeq.sorted
          println("sugg: " + suggestions)
          Result(suggestions, Seq.empty)
        case exactMatch =>
          println("exact: ")
          dom.console.log(exactMatch)
          val suggestions = js.Object.keys(data).filter(_.startsWith(text)).toSeq.sorted
          println("sugg: " + suggestions)
          val matches = exactMatch.asInstanceOf[js.Array[js.Dynamic]]
          Result(suggestions, matches.toSeq.map(toSymbolRef))
      }
      } catch {
        case x =>
          x.printStackTrace()
          Result(Seq.empty, Seq.empty)
      }
    }
  }
}

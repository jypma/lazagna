package zio.lazagna.location

import java.net.URLDecoder
import org.scalajs.dom
import Location._

/** Provides access to the current document's URL location */
object Location {
  val fragment = Fragment(dom.window.location.hash)

  case class Parameter(key: String, value: String)
  case class Fragment(parameterList: Seq[Parameter]) {
    val parameters: Map[String, String] = parameterList.map(p => p.key -> p.value).toMap
  }

  object Fragment {
    def apply(hash: String): Fragment = {
      if (hash.isEmpty()) Fragment(Seq.empty) else Fragment(
        hash.tail.split("&").toVector.map { param =>
          val parts = param.split("=", 2)
          if (parts.size != 2) None else Some(Parameter(URLDecoder.decode(parts(0), "UTF-8"), URLDecoder.decode(parts(1), "UTF-8")))
        }.collect { case Some(p) => p }
      )
    }
  }

}

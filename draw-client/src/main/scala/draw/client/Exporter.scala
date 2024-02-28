package draw.client

import zio.ZIO

import org.scalajs.dom

import scalajs.js
import zio.lazagna.dom.http.Request._

trait Exporter {
  def triggerExport: ZIO[Any, RequestError, Unit]
}

object Exporter {
  def make(svg: dom.svg.SVG) = {
    for {
      drawing <- ZIO.service[Drawing]
    } yield new Exporter {
      def triggerExport: ZIO[Any, RequestError, Unit] = {
        val symbols = svg.querySelectorAll("g.icon use").map(_.getAttribute("href")).flatMap(SymbolRef.parse).toSet
        for {
          svgSymbols <- ZIO.collectAll(symbols.map(_.category).map { category =>
            GET(AsDocument, category.href).map((category, _))
          })
          theme <- GET(AsString, "/theme-green.css")
        } yield {
          val viewBox = Drawing.Viewport.fit(svg.getBBox()).toSvgViewBox

          val icons = symbols.flatMap { s =>
            svgSymbols.find(_._1 == s.category)
              .flatMap(t => Option(t._2.getElementById(s.id)))
              .map(_.outerHTML)
          }.mkString("\n")
          // We only take the first style (style.css), not the theme. Inkscape doesn't do CSS variables,
          // https://gitlab.com/inkscape/inbox/-/issues/1180
          // so we'll apply a fixed theme.
          val cssVars = (for {
             m <- "(--.*?):(.*?);".r.findAllMatchIn(theme)
          } yield (m.group(1), m.group(2))).toSeq
          println(cssVars)
          val style = cssVars.foldLeft(dom.document.querySelector("html > head > style").innerHTML) { case (s, (key, value)) =>
            s.replace(s"var($key)", value)
          }

          // Inkscape doesn't understand rgba() either, so we'll do hex values instead.
          // https://gitlab.com/inkscape/inbox/-/issues/1195
          val style2 = "rgba\\( *([0-9]+) *, *([0-9]+) *, *([0-9]+) *\\)".r.replaceAllIn(style, { m =>
            val r = f"${m.group(1).toInt}%02X"
            val g = f"${m.group(1).toInt}%02X"
            val b = f"${m.group(1).toInt}%02X"
            s"#$r$g$b"
          })

          val data = new dom.XMLSerializer().serializeToString(svg)
            .replaceFirst("viewBox=\"(.*?)\"", s"viewBox=\"${viewBox}\"")
            .replace("<style/>", s"<style>${style2}</style>${icons}")
            .replaceAll("<use href=\"/symbols/(.*?)\\.svg#", "<use href=\"#")

          val options = new dom.BlobPropertyBag {}
          options.`type` = "image/svg+xml;charset=utf-8"
          val blob = new dom.Blob(js.Array(data), options)
          val url = dom.URL.createObjectURL(blob)

          val a = dom.document.createElement("a").asInstanceOf[dom.HTMLElement]
          a.setAttribute("download", "exported.svg")
          a.setAttribute("href", url)
          a.setAttribute("target", "_blank")

          a.click()
        }
      }
    }
  }
}

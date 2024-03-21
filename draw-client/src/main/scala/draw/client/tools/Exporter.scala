package draw.client.tools

import zio.ZIO
import zio.lazagna.dom.http.Request._

import draw.client.Drawing
import draw.client.render.RenderState
import draw.data.SymbolRef
import org.scalajs.dom

import scalajs.js

trait Exporter {
  def triggerExport: ZIO[Any, RequestError, Unit]
}

object Exporter {
  def make(svg: dom.svg.SVG) = {
    for {
      drawing <- ZIO.service[Drawing]
      renderState <- ZIO.service[RenderState]
    } yield new Exporter {
      def triggerExport: ZIO[Any, RequestError, Unit] = {
        val symbols = svg.querySelectorAll("g.icon use").map(_.getAttribute("href")).flatMap(SymbolRef.parse).toSet
        for {
          _ <- renderState.selectOnly(Set.empty)
          svgSymbols <- ZIO.collectAll(symbols.map(_.category).map { category =>
            GET(AsDocument, category.href).map((category, _))
          })
          theme <- GET(AsString, "/theme-green.css") // TODO: Use querySelectorAll("html > head > style")(1), so we actually apply the current theme.
        } yield {
          val viewBox = Drawing.Viewport.fit(svg.querySelector(".drawing").asInstanceOf[dom.SVGLocatable].getBBox(), 1.333).toSvgViewBox(1.333)

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
          } yield (m.group(1), m.group(2).trim())).toSeq
          val bgColor = cssVars.toMap.getOrElse("--bg-color", "#FFFFFF")

          val style = cssVars.foldLeft(dom.document.querySelectorAll("html > head > style").map(_.innerHTML).mkString("\n")) { case (s, (key, value)) =>
            s.replace(s"var($key)", value)
          }.replaceAll("--.*?;", "") // Remove the variable declarations themselves, now that we've replaced everything.
            // TODO: Use proper SVG filters and export them
            .replaceAll("filter:.*?;\n", "\n") // Inkscape doesn't do CSS filters anyway.

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
            .replaceFirst("<svg(.*?)>", "<svg$1 xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\">\n" +
              s"""<sodipodi:namedview id="namedview44" pagecolor="${bgColor}" inkscape:export-bgcolor="${bgColor}FF"/>""")
            .replace("<style/>", s"<style>${style2}</style>${icons}")
            .replaceAll("<use href=\"/symbols/(.*?)\\.svg#", "<use href=\"#")
            .replaceAll("indie_flowerregular", "Indie Flower") // TTF font name is different from OTF

          // We now have proper background, but command-line inkscape ignores the document background when exporting. It'll need to be explicitly set.

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

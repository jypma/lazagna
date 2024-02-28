package draw.client

import SymbolRef._

case class SymbolCategory(name: String) {
  val href = s"/symbols/${name}.svg"
}

case class SymbolRef(category: SymbolCategory, name: String) {
  val id = s"si-${name}"
  val href = s"${category.href}#${id}"
}

object SymbolRef {
  val person = SymbolRef(SymbolCategory("elusive"), "elusive-person")

  private val regex = "/symbols/(.*).svg#si-(.*)".r
  def parse(s: String): Option[SymbolRef] = {
    s match {
      case regex(category, name) => Some(SymbolRef(SymbolCategory(category), name))
      case _ => None
    }
  }
}

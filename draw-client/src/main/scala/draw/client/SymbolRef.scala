package draw.client

case class SymbolRef(category: String, name: String) {
  val href = s"/symbols/${category}.svg#si-${name}"
}

object SymbolRef {
  val person = SymbolRef("elusive", "elusive-person")
}

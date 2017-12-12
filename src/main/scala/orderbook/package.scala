
package object orderbook {

  object Instruction extends Enumeration {
    type Instruction = Value
    val Update, Delete, New = Value

    def parse(s: String): Value =
      s match {
        case "U" => Update
        case "D" => Delete
        case "N" => New
      }
  }

  object Side extends Enumeration {
    type Side = Value
    val Bid, Ask = Value

    def parse(s: String): Value =
      s match {
        case "B" => Bid
        case "A" => Ask
      }
  }

  import Side._

  def bookAsStr(book: OrderBook) = {

    def bookLevelAsStr(level: BookLevel): String = s"${level.price},${level.qty}"

    book.levels.foldLeft("")((acc, l) => acc + bookLevelAsStr(l) + (if (l.side == Ask) "\n" else ","))
  }

  def levelFromInputLine(l: InputLine, tickSize: Double) =
    BookLevel(
      l.side,
      l.priceLevelIdx,
      l.priceTicks * tickSize,
      l.qty
    )
}

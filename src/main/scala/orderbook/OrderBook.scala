package orderbook

import scala.io.Source
import orderbook.Instruction._
import orderbook.Side._

object OrderBook {

  def processInput(
    updateLines: Iterator[String],
    tickSize: Double,
    bookDepth: Int): OrderBook = {

    val emptyBook = OrderBook(
      tickSize,
      bookDepth,
      levels =
        Seq.range(1, bookDepth + 1)
           .flatMap(i => Seq(BookLevel(Ask, i), BookLevel(Bid, i))))

    updateLines
      .flatMap(parseInputLine)
      .foldLeft(emptyBook)((acc, line) => updateBook(acc, line))
  }

  def parseInputLine(line: String): Option[InputLine] = {
    val regex = """([U,D,N]) ([B,A]) ([0-9]+) ([0-9]+) ([0-9]+)""".r
    line match {
      case regex(instruction, side, priceLevelIdx, priceTicks, qty) =>
        Some(InputLine(
          Instruction.parse(instruction),
          Side.parse(side),
          priceLevelIdx.toInt,
          priceTicks.toInt,
          qty.toInt))
      case _ => None
    }
  }

  def updateBook(book: OrderBook, line: InputLine): OrderBook = {

    def matchLine(level: BookLevel) =
      level.side == line.side && level.priceLevelIdx == line.priceLevelIdx

    def levelSort(level: BookLevel) = (level.priceLevelIdx, level.side)

    def update(level: BookLevel) =
      book.copy(levels = (level +: book.levels.filterNot(matchLine)).sortBy(levelSort))

    def addAndShiftUp(level: BookLevel) = {
      val shiftedLevels = book.levels
        .map { l =>
          if (l.side == level.side && l.priceLevelIdx >= level.priceLevelIdx)
            l.copy(priceLevelIdx = l.priceLevelIdx + 1)
          else l
        }
        .filterNot(_.priceLevelIdx > book.depth)

      book.copy(levels = (level +: shiftedLevels).sortBy(levelSort))
    }

    def removeAndShiftDown(level: BookLevel) = {
      val shiftedLevels = book.levels
        .filterNot(matchLine)
        .map { l =>
          if (l.side == level.side && l.priceLevelIdx > level.priceLevelIdx)
            l.copy(priceLevelIdx = l.priceLevelIdx - 1)
          else l
        }

      book.copy(levels = (BookLevel(level.side, book.depth) +: shiftedLevels).sortBy(levelSort))
    }

    if (line.priceLevelIdx <= 0 || line.priceLevelIdx > book.depth)
      book
    else {
      val level = levelFromInputLine(line, book.tickSize)
      line.instruction match {
        case New => addAndShiftUp(level)
        case Update => update(level)
        case Delete => removeAndShiftDown(level)
      }
    }
  }

  def main(args: Array[String]): Unit =
    try {
      val (fileName, tickSize, bookDepth) = (args(0), args(1).toDouble, args(2).toInt)
      val updateLines = Source.fromFile(fileName).getLines
      println(bookAsStr(OrderBook.processInput(updateLines, tickSize, bookDepth)))
    } catch {
      case e: Exception => print("Usage: OrderBook <fileName> <tickSize> <bookDepth>")
    }
}

case class OrderBook(
  tickSize: Double,
  depth: Int,
  levels: Seq[BookLevel])

case class BookLevel(
  side: Side,
  priceLevelIdx: Int,
  price: Double = 0.0,
  qty: Int = 0)

case class InputLine(
  instruction: Instruction,
  side: Side,
  priceLevelIdx: Int,
  priceTicks: Int,
  qty: Int)
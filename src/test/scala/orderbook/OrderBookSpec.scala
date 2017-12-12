package orderbook

import org.scalatest._

class OrderBookSpec extends FlatSpec with Matchers {

  val inputAssignment = """
                           |N B 1 5 30
                           |N B 2 4 40
                           |N A 1 6 10
                           |N A 2 7 10
                           |U A 2 7 20
                           |U B 1 5 40
                         """
  import orderbook.Side._

  val outputAssignment = List(
    BookLevel(Bid, 1, 50.0, 40),
    BookLevel(Ask, 1, 60.0, 10),
    BookLevel(Bid, 2, 40.0, 40),
    BookLevel(Ask, 2, 70.0, 20)
  )

  val tickSize = 10.0

  private def splitLines(s: String): Iterator[String] =
    s.stripMargin
      .split("\n")
      .filterNot(_.trim == "")
      .iterator

  "Input from assignment" should "produce expected output" in {
    val in = splitLines(inputAssignment)
    val result = OrderBook.processInput(in, tickSize, bookDepth = 2)
    result should ===(OrderBook(tickSize, 2, outputAssignment))
  }

  "Input from assignment" should "produce expected output as string" in {
    val in = splitLines(inputAssignment)
    val result = OrderBook.processInput(in, tickSize, 2)

    bookAsStr(result) should ===(
      """
              |50.0,40,60.0,10
              |40.0,40,70.0,20
      """.stripMargin.trim + "\n")
  }

  "Input from assignment with tick size 1" should "produce expected output as string" in {
    val in = splitLines(inputAssignment)
    val result = OrderBook.processInput(in, tickSize = 1.0, bookDepth = 2)

    bookAsStr(result) should ===(
      """
        |5.0,40,6.0,10
        |4.0,40,7.0,20
      """.stripMargin.trim + "\n")
  }

  "Update instruction at price level outside kept range" should "be ignored" in {
    val in = splitLines(inputAssignment + "\n" + "U B 3 8 40")
    val result = OrderBook.processInput(in, tickSize, bookDepth = 2)
    result should ===(OrderBook(tickSize, 2, outputAssignment))
  }

  "Update instruction at price level 0" should "be ignored" in {
    val in = splitLines(inputAssignment + "\n" + "U B 0 8 40")
    val result = OrderBook.processInput(in, tickSize, bookDepth = 2)
    result should ===(OrderBook(tickSize, 2, outputAssignment))
  }

  "Processing input at book depth 1" should "produce book with a single price level" in {
    val in = splitLines(inputAssignment)
    OrderBook.processInput(in, tickSize, bookDepth = 1) should ===(OrderBook(tickSize, 1, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 60.0, 10))))
  }

  "Processing input at book depth 3" should "produce book with three price levels" in {
    val in = splitLines(inputAssignment)
    OrderBook.processInput(in, tickSize, bookDepth = 3) should ===(OrderBook(tickSize, 3, outputAssignment ::: List(
      BookLevel(Bid, 3, 0.0, 0),
      BookLevel(Ask, 3, 0.0, 0)
    )))
  }

  "Delete Bid instruction at maximum book depth" should "zero the bid values at requested price level" in {
    val in = splitLines(inputAssignment + "\n" + "D B 2 4 40")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 60.0, 10),
      BookLevel(Bid, 2, 0.0, 0),
      BookLevel(Ask, 2, 70.0, 20)
    )))
  }

  "Delete Ask instruction at maximum book depth" should "zero the ask values at requested price level. Input values in delete instructions are ignored" in {
    val in = splitLines(inputAssignment + "\n" + "D A 2 0 0")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 60.0, 10),
      BookLevel(Bid, 2, 40.0, 40),
      BookLevel(Ask, 2, 0.0, 0)
    )))
  }

  "Delete Bid instruction at non-maximum book depth" should "delete and shift values from higher level" in {
    val in = splitLines(inputAssignment + "\n" + "D B 1 5 40")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 40.0, 40),
      BookLevel(Ask, 1, 60.0, 10),
      BookLevel(Bid, 2, 0.0, 0),
      BookLevel(Ask, 2, 70.0, 20)
    )))
  }

  "Delete Ask instruction at non-maximum book depth" should "delete and shift values from higher level" in {
    val in = splitLines(inputAssignment + "\n" + "D A 1 0 0")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 70.0, 20),
      BookLevel(Bid, 2, 40.0, 40),
      BookLevel(Ask, 2, 0.0, 0)
    )))
  }

  "New Bid instruction at non-maximum book depth" should "insert and shift values up" in {
    val in = splitLines(inputAssignment + "\n" + "N B 1 6 15")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 60.0, 15),
      BookLevel(Ask, 1, 60.0, 10),
      BookLevel(Bid, 2, 50.0, 40),
      BookLevel(Ask, 2, 70.0, 20)
    )))
  }

  "New Ask instruction at non-maximum book depth" should "insert and shift values up" in {
    val in = splitLines(inputAssignment + "\n" + "N A 1 5 10")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 50.0, 10),
      BookLevel(Bid, 2, 40.0, 40),
      BookLevel(Ask, 2, 60.0, 10)
    )))
  }

  "New Ask instruction at maximum book depth" should "insert and shift values up" in {
    val in = splitLines(inputAssignment + "\n" + "N A 2 7 10")
    OrderBook.processInput(in, tickSize, bookDepth = 2) should ===(OrderBook(tickSize, 2, List(
      BookLevel(Bid, 1, 50.0, 40),
      BookLevel(Ask, 1, 60.0, 10),
      BookLevel(Bid, 2, 40.0, 40),
      BookLevel(Ask, 2, 70.0, 10)
    )))
  }

  "Input instruction N B 1 5 30" should "parse" in {
    OrderBook.parseInputLine("N B 1 5 30") should ===(Some(InputLine(Instruction.New, Side.Bid, 1, 5, 30)))
  }

  "Incorrect input instruction NB 1 5 30" should "parse" in {
    OrderBook.parseInputLine("NB 1 5 30") should ===(None)
  }

  "Incorrect input instruction N B 1 5B 30" should "not parse" in {
    OrderBook.parseInputLine("N B 1 5B 30") should ===(None)
  }

  "Input instruction N A 2 7 10" should "parse" in {
    OrderBook.parseInputLine("N A 2 7 10") should ===(Some(InputLine(Instruction.New, Side.Ask, 2, 7, 10)))
  }
}
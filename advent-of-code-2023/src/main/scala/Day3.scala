
package day3


case class Grid(
  numbers: IArray[IArray[Number]],
  symbols: IArray[IArray[Symbol]]
)

trait Element:
  def x: Int
  def length: Int

case class Symbol(x: Int, length: Int, charValue: Char) extends Element
case class Number(x: Int, length: Int, intValue: Int) extends Element

@main def main: Unit =
  val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day3" / "p1.txt"
  val lines: String = os.read(path)
  println(s"the answer is ${part1(lines)}")
  println(s"the answer is ${part2(lines)}")


def part1(input: String): Int =
  solution(input, findPartNumbers)

def part2(input: String): Int =
  solution(input, findGearRatios)

def parse(input: String): Grid =
  val (numbers, symbols) =
    IArray.from(input.linesIterator.map(parseRow(_))).unzip
  Grid(numbers = numbers, symbols = symbols)

def surrounds[E <: Element](y: Int, from: Element, rows: IArray[IArray[E]]): List[E] =
  val (x0, y0, x1, y1) = (from.x - 1, y - 1, from.x + from.length, y + 1)
  def overlaps(e: Element) = x0 <= (e.x + e.length - 1) && x1 >= e.x
  def findUp =
    if y0 < 0 then Nil
    else rows(y0).filter(overlaps).toList
  def findMiddle =
    rows(y).filter(overlaps).toList
  def findDown =
    if y1 >= rows.size then Nil
    else rows(y1).filter(overlaps).toList
  findUp ++ findMiddle ++ findDown

def solution(input: String, summarise: Grid => IterableOnce[Int]): Int =
  summarise(parse(input)).sum

def findPartNumbers(grid: Grid) =
  for
    (numbers, y) <- grid.numbers.iterator.zipWithIndex
    number <- numbers
    if surrounds(y, number, grid.symbols).sizeIs > 0
  yield
    number.intValue

def findGearRatios(grid: Grid) =
  for
    (symbols, y) <- grid.symbols.iterator.zipWithIndex
    symbol <- symbols
    if symbol.charValue == '*'
    combined = surrounds(y, symbol, grid.numbers)
    if combined.sizeIs == 2
  yield
    combined.map(_.intValue).product

def parseRow(row: String): (IArray[Number], IArray[Symbol]) =
  val buf = StringBuilder()
  val numbers = IArray.newBuilder[Number]
  val symbols = IArray.newBuilder[Symbol]
  var begin = -1 // -1 = not building an element, >= 0 = start of an element
  var knownSymbol = -1 // trinary: -1 = unknown, 0 = number, 1 = symbol
  def addElement(isSymbol: Boolean, x: Int, value: String) =
    if isSymbol then symbols += Symbol(x = x, length = value.size, charValue = value.head)
    else numbers += Number(x = x, length = value.size, intValue = value.toInt)
  for (curr, colIdx) <- row.zipWithIndex do
    val isSeparator = curr == '.'
    val inElement = begin >= 0
    val kindChanged =
      !inElement && !isSeparator
      || isSeparator && inElement
      || knownSymbol == 1 && curr.isDigit
      || knownSymbol == 0 && !curr.isDigit
    if kindChanged then
      if inElement then // end of element
        addElement(isSymbol = knownSymbol == 1, x = begin, value = buf.toString)
        buf.clear()
      if isSeparator then // reset all state
        begin = -1
        knownSymbol = -1
      else // begin new element
        begin = colIdx
        knownSymbol = if curr.isDigit then 0 else 1
        buf += curr
    else
      if !isSeparator then buf += curr
    end if
  end for
  if begin >= 0 then // end of line
    addElement(isSymbol = knownSymbol == 1, x = begin, value = buf.toString)
  (numbers.result(), symbols.result())
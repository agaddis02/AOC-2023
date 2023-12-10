package day10

import scala.util.control.Breaks._
import scala.collection.mutable
import scala.util.matching.Regex

case class Tile(symbol: Char, pos: Point):
    val neighbours: Set[Point] =
    symbol match
        case '|' => Set(pos + North, pos + South)
        case '-' => Set(pos + East, pos + West)
        case 'L' => Set(pos + North, pos + East)
        case 'J' => Set(pos + North, pos + West)
        case '7' => Set(pos + South, pos + West)
        case 'F' => Set(pos + South, pos + East)
        case _   => Set.empty

case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def -(other: Point): Point = Point(x - other.x, y - other.y)

val North = Point(0, -1)
val South = Point(0, 1)
val West = Point(-1, 0)
val East = Point(1, 0)

val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day10" / "p1.txt"

@main def part1() =
    val input = os.read(path)
    val (loop, _) = findLoopFixTiles(parseTiles(input))
    println(loop.size / 2)

@main def part2() =
    val input = os.read(path)
    val (loop, tiles) = findLoopFixTiles(parseTiles(input))
    val loopByY = loop.groupBy(_.pos.y).map((y, tiles) => (y, tiles.toSeq.sortBy(_.pos.x)))
    val loopSet = loop.toSet

    val cornerPairs = Map('F' -> '7', 'L' -> 'J')
    val output = tiles.values.filter(!loopSet.contains(_)).count(t => {
        val leftWalls = loopByY.get(t.pos.y).getOrElse(Seq.empty).filter(_.pos.x < t.pos.x)
        val wallCount = leftWalls.foldLeft((0, '.')):
            case ((count, prevCorner), wall) =>
            wall.symbol match
                case '|'                                         => (count + 1, prevCorner)
                case c if cornerPairs.get(prevCorner) == Some(c) => (count - 1, c)
                case c: ('L' | 'F')                              => (count + 1, c)
                case c: ('7' | 'J')                              => (count, c)
                case _                                           => (count, prevCorner)
        wallCount._1 % 2 == 1
    })

    println(output)

def findLoopFixTiles(tiles: Map[Point, Tile]) =
    val start = tiles.values.find(t => t.symbol == 'S').get
    val neighbours = Seq(North, East, South, West)
        .flatMap(d => tiles.get(start.pos + d))
        .filter(_.neighbours.contains(start.pos))
    val neighbourPoints = neighbours.map(_.pos).toSet
    val fixedStart = "|-LJ7F".map(Tile(_, start.pos)).find(t => t.neighbours == neighbourPoints).get
    val fixedTiles = tiles ++ Map(fixedStart.pos -> fixedStart)
    val loop = mutable.ArrayBuffer[Tile](fixedStart)
    var current = neighbours.head
    while (current != fixedStart) do
      val next = fixedTiles(current.neighbours.find(_ != loop.last.pos).get)
      loop += current
      current = next

    (loop.toSeq, fixedTiles)

def parseTiles(input: String) =
    input.linesIterator.zipWithIndex
        .flatMap((l, y) => l.zipWithIndex.map((t, x) => (t, x, y)))
        .map((t, x, y) => Point(x, y) -> Tile(t, Point(x, y)))
        .toMap


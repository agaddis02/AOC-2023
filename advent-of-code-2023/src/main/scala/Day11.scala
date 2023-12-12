package day11

import scala.util.control.Breaks._
import scala.collection.mutable

val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day11" / "p1.txt"

type Position = (Long, Long)
type Universe = Seq[Position]

@main def part1() =
    println(s"Part 1: ${allDistancesSum(path, 1)}")

@main def part2() =
    println(s"Part 2: ${allDistancesSum(path, 1_000_000)}")
//     return
    // test("part 2 sample 1"):
    //     assertEquals(allDistancesSum("day11-sample.txt", 10), 1030L)
    // test("part 2 sample 2"):
    //     assertEquals(allDistancesSum("day11-sample.txt", 100), 8410L)
    // test("part 2"):
    //     assertEquals(allDistancesSum("day11.txt", 1_000_000L), 678626199476L)


val manhattanDistance = (p1: Position, p2: Position)  =>
    math.abs(p1(0) - p2(0)) + math.abs(p1(1) - p2(1))

def getInput(name: os.Path): Universe =
    val lines = os.read.lines(name).toVector
    for
        (row, rowNumber) <- lines.zipWithIndex
        (cell, columnNumber) <- row.zipWithIndex
        if cell == '#'
    yield (rowNumber, columnNumber)

  /// part 1

def expand(universe: Universe, factor: Long): Universe =
    val maxRow = universe.map(_(0)).max
    val emptyRows = 0L.to(maxRow).filterNot(r => universe.exists(_(0) == r))
    val maxCol = universe.map(_(1)).max
    val emptyCols = 0L.to(maxCol).filterNot(c => universe.exists(_(1) == c))
    for (row, col) <- universe
    yield (row + (factor - 1) * (emptyRows.count(_ < row)),
           col + (factor - 1) * (emptyCols.count(_ < col)))

def allDistancesSum(name: os.Path, factor: Long): Long =
    val universe = expand(getInput(name), factor)
    (for case Seq(p1, p2) <- universe.combinations(2)
        yield manhattanDistance(p1, p2)).sum

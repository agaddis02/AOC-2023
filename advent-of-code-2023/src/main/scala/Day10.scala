package day10

import scala.util.control.Breaks._
import scala.collection.mutable


val pipeMoves: Map[Char, Set[(Int, Int)]] = Map(
    '|' -> Set((-1, 0), (1, 0)),   // Vertical pipe: Up and Down
    '-' -> Set((0, -1), (0, 1)),   // Horizontal pipe: Left and Right
    'L' -> Set((-1, 0), (0, 1)),   // L-shaped pipe: Up and Right
    'J' -> Set((-1, 0), (0, -1)),  // J-shaped pipe: Up and Left
    '7' -> Set((1, 0), (0, -1)),   // 7-shaped pipe: Down and Left
    'F' -> Set((1, 0), (0, 1))     // F-shaped pipe: Down and Right
    // Note: 'S' and '.' will be handled separately in the DFS function
)

val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day10" / "p1.txt"

@main def Part1: Unit =
    val input: String = os.read(path)
    val grid: Array[Array[Char]] = input.linesIterator.map(_.toCharArray).toArray


    // Find the position of 'S'
    val (sX, sY) = findS(grid)

    // Print original grid
    println("Original Grid:")
    // grid.foreach(row => println(row.mkString))

    // Initialize distances array with high values
    val distances = Array.fill(grid.length, grid(0).length)(Int.MaxValue)

    println(s"Starting position: ($sX, $sY)")
    println(s"Starting distance: ${distances}")

@main def Part2: Unit =
    return

// Function to find the position of 'S'
def findS(grid: Array[Array[Char]]): (Int, Int) = {
    var position = (-1, -1)
    breakable {
        for (i <- grid.indices; j <- grid(i).indices) {
            if (grid(i)(j) == 'S')  {
                position = (i, j)
                break()
            }
        }
    }
    position // Return an invalid position if 'S' is not found
}



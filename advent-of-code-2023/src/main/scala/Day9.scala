package day9

import scala.util.control.Breaks._

case class Differences(diffs: List[List[Int]])

case class History(readings: List[Int], differences: Differences = Differences(List.empty))

val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day9" / "p1.txt"

@main def Part1: Unit =
    val input: String = os.read(path)
    val histories: List[History] = input.linesIterator.map(line => {
        History(line.split(" ").map(_.toInt).toList)
    }).map(history => {
        val differences = findDifferences(history.readings)
        History(history.readings, Differences(differences))
    }).toList
    for (history <- histories) {
        println(history)
    }

    var output = 0
    for (history <- histories) {
        val nextValue = findNextValue(history.differences)
        output += nextValue
    }
    println(output)

@main def Part2: Unit =
    val input: String = os.read(path)
    val histories: List[History] = input.linesIterator.map(line => {
        History(line.split(" ").map(_.toInt).toList)
    }).map(history => {
        val differences = findDifferences(history.readings)
        History(history.readings, Differences(differences))
    }).toList
    for (history <- histories) {
        println(history)
    }

    var output = 0
    for (history <- histories) {
        val nextValue = findPreviousValue(history.differences)
        output += nextValue
    }
    println(output)


def findDifferences(numbers: List[Int]): List[List[Int]] = {
    if (numbers.isEmpty || numbers.forall(_ == 0)) {
        List(numbers)
    } else {
        val pairs = numbers.sliding(2).toList
        val differences = pairs.map(pair => pair(1) - pair(0))
        numbers :: findDifferences(differences)
    }
}

def findNextValue(differences: Differences): Int = {

    val backwardsRange = differences.diffs.length - 1 to 0 by -1
    println(backwardsRange)
    var products: Array[Int] = Array()
    var diffs = differences.diffs

    breakable {
        for (i <- backwardsRange) {
            val j = i - 1
            if (j < 0){
                break()
            }
            println(s"i: $i, j: $j")
            val currDiff = diffs(i)
            val nextDiff = diffs(j)
            println(s"diff: $currDiff")
            println(s"nextDiff: $nextDiff")
            val product = currDiff(currDiff.length - 1) + nextDiff(nextDiff.length - 1)
            diffs = diffs.updated(j, nextDiff :+ product)
            products = products :+ product
        }
    }

    products(products.length - 1)
}

def findPreviousValue(differences: Differences): Int = {
    val backwardsRange = differences.diffs.length - 1 to 0 by -1
    var products: Array[Int] = Array()
    var diffs = differences.diffs

    breakable {
        for (i <- backwardsRange) {
            val j = i - 1
            if (j < 0){
                break()
            }
            println(s"i: $i, j: $j")
            val currDiff = diffs(i)
            val nextDiff = diffs(j)
            println(s"diff: $currDiff")
            println(s"nextDiff: $nextDiff")
            val product = nextDiff(0) - currDiff(0) 
            diffs = diffs.updated(j, product :: nextDiff )
            products = products :+ product
        }
    }
    println(products.toList)
    products(products.length - 1)
}
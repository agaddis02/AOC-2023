package day8

import scala.util.control.Breaks._
import scala.annotation.tailrec

enum Direction:
    case L, R

case class Node(element: String, left: String, right: String)

@main def Part1: Unit =
    val startingNode = "AAA"
    val destinationNode = "ZZZ"
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day8" / "p1.txt"
    val input: String = os.read(path)

    val directions: List[Direction] = input.split("\n", 2)(0).trim().map(character => {
        if (character == 'L') {
            Direction.L
        } else {
            Direction.R
        }
    }).toList

    val nodes = input.split("\n", 3)(2).linesIterator.map(line => {
        val element = line.split("=")(0).trim()
        val left = line.split("\\(")(1).split(",")(0).trim()
        val right = line.split(",")(1).split("\\)")(0).trim()
        Node(element, left, right)
    }).toList
    // println(nodes)


    var found = false
    var steps = 0
    var currentNode = startingNode

    println(s"Starting Node: $startingNode, Destination Node: $destinationNode")
    println(s"Directions: $directions")

    while (!found) {
        for (direction <- directions) {
            steps = steps + 1
            println(s"Current Node: $currentNode, Direction: $direction")
            currentNode = findNextNode(currentNode, nodes, direction)
        }
        if (currentNode == destinationNode) {
            found = true
        } 
    }
    println(steps)

val findNextNode = (currentNode: String, nodes: List[Node], direction: Direction) => {
    val node = nodes.find(node => node.element == currentNode).get
    if (direction == Direction.L) {
        node.left
    } else {
        node.right
    }
}

@main def Part2: Unit =
    
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day8" / "p1.txt"
    val input: String = os.read(path)
    val directions: List[Direction] = input.split("\n", 2)(0).trim().map(character => {
        if (character == 'L') {
            Direction.L
        } else {
            Direction.R
        }
    }).toList

    val nodes = input.split("\n", 3)(2).linesIterator.map(line => {
        val element = line.split("=")(0).trim()
        val left = line.split("\\(")(1).split(",")(0).trim()
        val right = line.split(",")(1).split("\\)")(0).trim()
        Node(element, left, right)
    }).toList

    // Convert list of nodes to a Map, faster lkup
    val nodeMap = nodes.map(node => node.element -> node).toMap

    val startingNodes = nodes.filter(node => node.element.endsWith("A"))

    val stepsForEachPath = startingNodes.map { startNode =>
        val numSteps = countSteps(startNode.element, directions, nodeMap)
        numSteps.toLong
    }

    val lcmOfSteps = lcm(stepsForEachPath)

    println(s"Least Common Multiple of Steps: $lcmOfSteps")

def countSteps(startNode: String, directions: List[Direction], nodeMap: Map[String, Node]): Int = {
    @tailrec
    def helper(currentNode: String, directionsCycle: Iterator[Direction], count: Int): Int = {
        if (currentNode.endsWith("Z")) count
        else {
            val node = nodeMap(currentNode)
            val nextNode = if (directionsCycle.next() == Direction.L) node.left else node.right
            helper(nextNode, directionsCycle, count + 1)
        }
    }

    helper(startNode, Stream.continually(directions).flatten.iterator, 0)
}

def lcm(numbers: List[Long]): Long = {
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
    def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

    numbers.foldLeft(1L)((a, b) => lcm(a, b))
}

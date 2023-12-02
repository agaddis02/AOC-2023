package day1

import scala.util.control.Breaks._

val wordsToNums = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
)
val reversedWordsToNums = wordsToNums.map { case (key, value) => (key.reverse, value) }

@main def hello: Unit =
  val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day1" / "p1.txt"
  val lines: Seq[String] = os.read.lines(path)
  var p1values: List[Int] = List()
  var p2Values: List[Int] = List()
  for (line <- lines) {
    println(line)
    // println(calibrateValues(line).join(""))
    val pair: Int = calibrateValues(line).mkString("").toInt
    val pair2: Int = calibrateValuesWStrings(line).mkString("").toInt
    p1values = pair :: p1values
    p2Values = pair2 :: p2Values
  }
  println(p1values.sum)
  println(p2Values.sum)


val findDigit = (line: String, acceptWords: Boolean) => {
  var digit = -1
  var found = false
  var currentWord: String = ""

  breakable {
      for (i <- 0 to line.length - 1) {
        val char = line.charAt(i)
        currentWord = currentWord + char

        if (acceptWords && !found) {
            for (word <- wordsToNums.keys) {
              if ((currentWord.contains(word) || currentWord.contains(word.reverse))  && !found) {
                val num = wordsToNums(word)
                digit = num
                currentWord = ""
                found = true
                break()
              }
          }
        }
        if (char.isDigit && !found) {
          val num = char.asDigit
          println("Found Digit" + num)
          digit = num
          found = true
          break()
        }
    }
  }
  digit
}

val calibrateValues = (line: String) => {
  val reversedLine: String = line.reverse

  // find the first digit going through
  val firstDigit = findDigit(line, false)

  // find the second digit, by going in reverse
  val secondDigit = findDigit(reversedLine, false)

  val output: List[Int] = List(firstDigit,secondDigit)
  output
}
val calibrateValuesWStrings = (line: String) => {
 // 54431
  val reversedLine: String = line.reverse
  // find the first digit going through
  val firstDigit = findDigit(line, true)
  // find the second digit, by going in reverse
  val secondDigit = findDigit(reversedLine, true)

  val output: List[Int] = List(firstDigit, secondDigit)
  output
}
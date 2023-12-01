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


val calibrateValues = (line: String) => {
  var output: List[Int] = List()
  var firstDigit: Int = 0
  var secondDigit: Int = 0
  var found = false

  // find the first digit going through
  for (i <- 0 to line.length - 1) {
    val char = line.charAt(i)
    // println(char)
    // print(i)
    var output: List[Int] = List()
    if (char.isDigit && !found) {
      val num = char.asDigit
      println("First Digit: " + num)
      firstDigit = num
      found = true
    }
  }
  // find the second digit, by going in reverse
  found = false
  val reversedLine: String = line.reverse
  for (i <- 0 to reversedLine.length - 1) {
    val char = reversedLine.charAt(i)
    // println(char)
    if (char.isDigit && !found) {
      val num = char.asDigit
      println("Second Digit: " + num)
      secondDigit = num
      found = true
    }
  }
  output = firstDigit :: secondDigit :: output
  output
}


val calibrateValuesWStrings = (line: String) => {
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
  
  var output: List[Int] = List()
  var firstDigit: Int = 0
  var secondDigit: Int = 0
  var found = false
  var currentWord: String = ""
  

  // find the first digit going through
  for (i <- 0 to line.length - 1) {
    val char = line.charAt(i)
    currentWord = currentWord + char
    for (word <- wordsToNums.keys) {
      if (currentWord.contains(word) && !found) {
        val num = wordsToNums(word)
        println("First Digit: " + num)
        firstDigit = num
        currentWord = ""
        found = true
      }
    }
    if (char.isDigit && !found) {
      val num = char.asDigit
      println("First Word Digit: " + num)
      firstDigit = num
      found = true
    }
  }

  // find the second digit, by going in reverse
  found = false
  currentWord = ""
  val reversedLine: String = line.reverse
  
  for (i <- 0 to reversedLine.length - 1) {
    val char = reversedLine.charAt(i)
    currentWord = currentWord + char
    // checks for a word
    for (word <- reversedWordsToNums.keys) {
      if (currentWord.contains(word) && !found) {
        val num = reversedWordsToNums(word)
        println("Second Word Digit: " + num)
        secondDigit = num
        currentWord = ""
        found = true
      }
    }
    // checks for a normal digit
    if (char.isDigit && !found) {
      val num = char.asDigit
      println("Second Digit: " + num)
      secondDigit = num
      found = true
    }
  }
  output = firstDigit :: secondDigit :: output
  output
}
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

package day1

class Day1Test extends munit.FunSuite {

  val p1Inputs: List[String] = List(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  )
  val p2Inputs: List[String] = List(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  )
  val p1Expected: List[Int] = List(12,38,15,77)
  val p2Expected: List[Int] = List(29, 83, 13, 24, 42, 14, 76)
  

  test("Part 1") {
    val inputsAndExpectes: Map[String, Int] = p1Inputs.zip(p1Expected).toMap
    inputsAndExpectes.foreach((input, expected) => {
      val obtained = calibrateValues(input).mkString("").toInt
      assertEquals(obtained, expected)
    })
  }

  test("Part 2") {
    val inputsAndExpectes: Map[String, Int] = p2Inputs.zip(p2Expected).toMap
    inputsAndExpectes.foreach((input, expected) => {
      val obtained = calibrateValuesWStrings(input).mkString("").toInt
      assertEquals(obtained, expected)
    })
  }
}

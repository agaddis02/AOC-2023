// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html


class Day1Test extends munit.FunSuite:

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
      val obtained = day1.calibrateValues(input).mkString("").toInt
      assertEquals(obtained, expected)
    })
  }

  test("Part 2") {
    val inputsAndExpectes: Map[String, Int] = p2Inputs.zip(p2Expected).toMap
    inputsAndExpectes.foreach((input, expected) => {
      val obtained = day1.calibrateValuesWStrings(input).mkString("").toInt
      assertEquals(obtained, expected)
    })
  }
end Day1Test

class Day2Test extends munit.FunSuite:
    val inputs: List[String] = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    test("Part 1") {
      val expected: List[Int] = List(1, 2, 0, 0, 5)

      val inputsAndExpectes: Map[String, Int] = inputs.zip(expected).toMap

      inputsAndExpectes.foreach((input, expected) => {
        val obtained = day2.part1(input)
        assertEquals(obtained, expected)
      })
    }

    test("Part 2") {

      val expected: List[Int] = List(48, 12, 1560, 630, 36)

      val inputsAndExpectes: Map[String, Int] = inputs.zip(expected).toMap
      inputsAndExpectes.foreach((input, expected) => {
        val obtained = day2.part2(input)
        assertEquals(obtained, expected)
      })

    }
end Day2Test
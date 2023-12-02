// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

package day2

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
        val obtained = part1(input)
        assertEquals(obtained, expected)
      })
    }

    test("Part 2") {

      val expected: List[Int] = List(48, 12, 1560, 630, 36)

      val inputsAndExpectes: Map[String, Int] = inputs.zip(expected).toMap
      inputsAndExpectes.foreach((input, expected) => {
        val obtained = part2(input)
        assertEquals(obtained, expected)
      })

    }
end Day2Test

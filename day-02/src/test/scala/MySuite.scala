// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

package day2

class Day2Test extends munit.FunSuite:
    val input1: String = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    val input2: String = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    val input3: String = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    val input4: String = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    val input5: String = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

    test("Part 1") {

      val expected1: Int = 1
      val expected2: Int = 2
      val expected3: Int = 0
      val expected4: Int = 0
      val expected5: Int = 5

      val inputsAndExpectes: Map[String, Int] = Map(
        input1 -> expected1,
        input2 -> expected2,
        input3 -> expected3,
        input4 -> expected4,
        input5 -> expected5
      )

      inputsAndExpectes.foreach((input, expected) => {
        val obtained = part1(input)
        assertEquals(obtained, expected)
      })
    }

    test("Part 2") {

      val expected1: Int = 48
      val expected2: Int = 12
      val expected3: Int = 1560
      val expected4: Int = 630
      val expected5: Int = 36

      val inputsAndExpectes: Map[String, Int] = Map(
        input1 -> expected1,
        input2 -> expected2,
        input3 -> expected3,
        input4 -> expected4,
        input5 -> expected5
      )
      inputsAndExpectes.foreach((input, expected) => {
        val obtained = part2(input)
        assertEquals(obtained, expected)
      })

    }
end Day2Test

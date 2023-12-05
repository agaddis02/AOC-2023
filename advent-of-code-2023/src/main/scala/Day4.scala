package day4

case class Card(Number: Int, WinningNumbers: List[Int], MyNumbers: List[Int])

@main def main: Unit =
  val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day4" / "p1.txt"
  val lines: Seq[String] = os.read.lines(path)
  var total = 0
  for (line <- lines)
    println(line)
    total += part1(line)
  println("Part 1: " + total)
  val cardsFinal = part2(lines)
  println("Part 2: " + cardsFinal)




val part1 = (line: String) =>
  val card = cleanCard(line)
  val intersectingValues = card.MyNumbers.intersect(card.WinningNumbers)
  
  
  var totalPoints = 1
  if (intersectingValues.isEmpty)
    0
  else
    intersectingValues.indices.foreach(i => {
      if (i == 0)
        print("")
      else
        totalPoints = totalPoints + totalPoints
    })
    totalPoints
  

def cleanCard(card: String): Card =
  val cardNumber: Int = card.split(":")(0).split("\\s+")(1).trim().toInt
  val winningNumbers: List[Int] = card.split(":")(1).trim().split("""\|""")(0).trim().split("\\s+").map(num => num.toInt).toList
  val myNumbers: List[Int] = card.split(":")(1).trim().split("""\|""")(1).trim().split("\\s+").map(num => num.toInt).toList

  println(s"cardNumber: $cardNumber")
  println(s"winningNumbers: $winningNumbers")
  println(s"myNumbers: $myNumbers")

  val output = Card(cardNumber, winningNumbers, myNumbers)

  return output


def part2(pile: Seq[String]): Int =
  val cards: List[Card] = pile.toList.map(row => cleanCard(row))

  val cardCount = Array.fill(cards.length)(1)

  for (i <- cards.indices) {
    val matchingNumbers = cards(i).MyNumbers.intersect(cards(i).WinningNumbers).length
    for (j <- (i + 1) until (i + matchingNumbers + 1) if j < cardCount.length) {
      cardCount(j) += cardCount(i)
    }
  }


  // return generatedCards.toList

  return cardCount.sum
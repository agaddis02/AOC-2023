package day7

import scala.util.control.Breaks._

// p1
// enum CardLabel:
//     case Two, Three, Four, Five, Six, Seven, Eight, Nine, T, J, Q, K, A

// p2 
enum CardLabel:
    case J, Two, Three, Four, Five, Six, Seven, Eight, Nine, T, Q, K, A
enum HandType:
    case Unknown, HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
case class Card(label: CardLabel)
case class Hand(cards: List[Card], bid: Int, t: HandType = HandType.Unknown, rank: Int = 0)

val valueToEnum: Map[Char, CardLabel] = Map(
    '2' -> CardLabel.Two,
    '3' -> CardLabel.Three,
    '4' -> CardLabel.Four,
    '5' -> CardLabel.Five,
    '6' -> CardLabel.Six,
    '7' -> CardLabel.Seven,
    '8' -> CardLabel.Eight,
    '9' -> CardLabel.Nine,
    'T' -> CardLabel.T,
    'J' -> CardLabel.J,
    'Q' -> CardLabel.Q,
    'K' -> CardLabel.K,
    'A' -> CardLabel.A,
)

def rerankHands(hands: List[Hand]): List[Hand] = {
    hands.sortWith { (hand1, hand2) =>
        if (hand1.t == hand2.t) {
            compareCards(hand1.cards, hand2.cards)
        } else {
            hand1.t.ordinal > hand2.t.ordinal
        }
    }.reverse
}

def compareCards(cards1: List[Card], cards2: List[Card]): Boolean = {
    // Assuming both lists are of the same length
    var result = false
    breakable {
        for (i <- cards1.indices) {
            if (cards1(i).label.ordinal != cards2(i).label.ordinal) {
                result = cards1(i).label.ordinal > cards2(i).label.ordinal
                break()
            }
        }
    }
    result
}

@main def Part1: Unit =
    val getHandType = (hand: Hand) => {
        val cards = hand.cards
        val cardCountMapping = cards.groupBy(card => card.label).map((label, cards) => (label, cards.size))
        // cardCountMapping.size
        cardCountMapping.values.toList
        // 1: 5
        if (cardCountMapping.size == 1) {
            HandType.FiveOfAKind
        }
        // 1: 4, 2: 1
        else if (cardCountMapping.size == 2 && cardCountMapping.values.toList.contains(4)) {
            HandType.FourOfAKind
        }
        // 1: 3, 2: 2
        else if (cardCountMapping.size == 2 && cardCountMapping.values.toList.contains(3) && cardCountMapping.values.toList.contains(2)) {
            HandType.FullHouse
        }
        // 1: 3, 2: 1, 3: 1
        else if (cardCountMapping.size == 3 && cardCountMapping.values.toList.contains(3)) {
            HandType.ThreeOfAKind
        }
        // 1: 2, 2: 2, 3: 1
        else if (cardCountMapping.size == 3 && cardCountMapping.values.toList.map(cnt => cnt == 2).count(_ == true) == 2) {
            HandType.TwoPair
        }
        // 1: 2, 2: 1, 3: 1, 4: 1
        else if (cardCountMapping.size == 4) {
            HandType.OnePair
        }
        // 1: 1, 2: 1, 3: 1, 4: 1, 5: 1
        else if (cardCountMapping.size == 5){
            HandType.HighCard
        }
        else {
            HandType.Unknown
        }
    }
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day7" / "p1.txt"
    val input: String = os.read(path)
    val bids: List[Int] = input.linesIterator.map(line => line.split(" ")(1).toInt).toList
    val hands: List[Hand] = input.linesIterator.map(line => line.split(" ")(0)).toList.zip(bids).map((hand, bid) => {
        val cards: List[Card] = hand.map(character => Card(valueToEnum.get(character).get)).toList
        val handType = getHandType(Hand(cards, bid))
        val h = Hand(cards, bid, t = handType)
        h
    }).zipWithIndex.map((hand, index) => {
        val h = Hand(hand.cards, hand.bid, hand.t, index + 1)
        h
    })
    val rankedHands = rerankHands(hands).zipWithIndex.map((hand, index) => {
        val h = Hand(hand.cards, hand.bid, hand.t, index + 1)
        h
    })
    var totalWinnings = 0
    for (hand <- rankedHands) {
        println(hand)
        val winnings = hand.rank * hand.bid
        totalWinnings = totalWinnings + winnings
    }
    println(s"Part 1: $totalWinnings")


@main def Part2: Unit =
    def determineHandTypeFromMap(cardCountMapping: Map[CardLabel, Int]): HandType = {
        if (cardCountMapping.size == 1) {
            HandType.FiveOfAKind
        }
        else if (cardCountMapping.size == 2 && cardCountMapping.values.toList.contains(4)) {
            HandType.FourOfAKind
        }
        else if (cardCountMapping.size == 2 && cardCountMapping.values.toList.contains(3) && cardCountMapping.values.toList.contains(2)) {
            HandType.FullHouse
        }
        else if (cardCountMapping.size == 3 && cardCountMapping.values.toList.contains(3)) {
            HandType.ThreeOfAKind
        }
        else if (cardCountMapping.size == 3 && cardCountMapping.values.toList.map(cnt => cnt == 2).count(_ == true) == 2) {
            HandType.TwoPair
        }
        else if (cardCountMapping.size == 4) {
            HandType.OnePair
        }
        else if (cardCountMapping.size == 5){
            HandType.HighCard
        }
        else {
            HandType.Unknown
        }
    }
    def generateAllPossibleHands(cards: List[Card], jCount: Int): List[Map[CardLabel, Int]] = {
        val nonJCards = cards.filter(_.label != CardLabel.J)
        val nonJCardLabels = nonJCards.map(_.label)
        val allCardLabels = CardLabel.values.filter(_ != CardLabel.J)

        (0 to jCount).flatMap { jUsed =>
            allCardLabels.flatMap { cardLabel =>
                val currentHandLabels = nonJCardLabels ++ List.fill(jUsed)(cardLabel)
                val cardCountMapping = currentHandLabels.groupBy(identity).view.mapValues(_.size).toMap
                Some(cardCountMapping)
            }
        }.toList
    }


    val getHandType = (hand: Hand) => {
        val jCount = hand.cards.count(_.label == CardLabel.J)
        val possibleCardCountMappings = generateAllPossibleHands(hand.cards, jCount)

        val possibleHandTypes = possibleCardCountMappings.map(determineHandTypeFromMap)
        possibleHandTypes.maxBy(_.ordinal) // Assuming each HandType has a priority value
    }

    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day7" / "p1.txt"
    val input: String = os.read(path)
    val bids: List[Int] = input.linesIterator.map(line => line.split(" ")(1).toInt).toList
    val hands: List[Hand] = input.linesIterator.map(line => line.split(" ")(0)).toList.zip(bids).map((hand, bid) => {
        val cards: List[Card] = hand.map(character => Card(valueToEnum.get(character).get)).toList
        val handType = getHandType(Hand(cards, bid))
        val h = Hand(cards, bid, t = handType)
        h
    }).zipWithIndex.map((hand, index) => {
        val h = Hand(hand.cards, hand.bid, hand.t, index + 1)
        h
    })
    val rankedHands = rerankHands(hands).zipWithIndex.map((hand, index) => {
        val h = Hand(hand.cards, hand.bid, hand.t, index + 1)
        h
    })
    var totalWinnings = 0
    for (hand <- rankedHands) {
        println(hand)
        val winnings = hand.rank * hand.bid
        totalWinnings = totalWinnings + winnings
    }
    println(s"Part 2: $totalWinnings")
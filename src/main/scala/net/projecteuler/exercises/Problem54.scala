package net.projecteuler.exercises

import scala.io.Source

/**
 * @author Andreas C. Osowski
 */
object Problem54 extends App {

  case class Card(value: Int, suit: Char) {
    assert(value >= 2 && value <= 14)
    assert(Array('H', 'C', 'S', 'D').contains(suit))
  }

  object HandType {
    val RoyalFlush = 10
    val StraightFlush = 9
    val FourOfAKind = 8
    val FullHouse = 7
    val Flush = 6
    val Straight = 5
    val ThreeOfAKind = 4
    val TwoPairs = 3
    val OnePair = 2
    val HighCard = 1

  }

  case class Hand(cards: List[Card]) {
    import HandType._

    assert(cards.length == 5)

    lazy val sorted = cards.sortBy(_.value).reverse
    lazy val values = sorted.map(_.value)
    lazy val flush = cards.tail.forall(c => c.suit == cards.head.suit)

    // Ace can't be the lowest card as per the rules.
    lazy val straight = sorted.zipWithIndex.forall {
      case (c, i) => (c.value + i) == sorted.head.value
    }

    // We can't use groupBy due to type issues and only need the count really anyway.
    def ranking: List[Int] = values.distinct.map(c => (values.count(c ==), c)).sorted.reverse match {
      case _ if flush && values.sum == 60 => RoyalFlush :: Nil
      case _ if flush && straight => StraightFlush :: values
      case List((4, a), (1, b)) => List(FourOfAKind, a, b)
      case List((3, a), (2, b)) => List(FullHouse, a, b)
      case _ if flush => Flush :: values
      case _ if straight => Straight :: values
      case List((3, a), (1, b), (1, c)) => List(ThreeOfAKind, a, b, c)
      case List((2, a), (2, b), (1, c)) => List(TwoPairs, a, b, c)
      case List((2, a), (1, b), (1, c), (1, d)) => List(OnePair, a, b, c, d)
      case _ => HighCard :: values
    }
  }

  type Session = (Hand, Hand)

  def parseLine(line: String): Session = {
    val session = line.split(" ").grouped(5).map {
      h =>
        Hand(h.map(c => Card(c(0) match {
          case 'T' => 10
          case 'J' => 11
          case 'Q' => 12
          case 'K' => 13
          case 'A' => 14
          case c => c.asDigit
        }, c(1))).toList)
    }.toList
    (session(0), session(1))
  }

  override def main(args: Array[String]) = {
    val source = Source.fromFile(args(0))
    val sessions = source.getLines.map(parseLine)

    println(sessions.count(session =>
      session._1.ranking.zip(session._2.ranking).dropWhile(r => r._1 == r._2) match {
        case (a, b) :: _ => a > b
        case _ => false
      }
    ))
    source.close()
  }


}

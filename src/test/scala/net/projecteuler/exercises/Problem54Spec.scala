package net.projecteuler.exercises

import org.scalatest.{FlatSpec, Matchers}

/**
 * @author Andreas C. Osowski
 */
class Problem54Spec extends FlatSpec with Matchers {

  import Problem54._
  import HandType._

  val sampleCards = List(Card(3, 'C'), Card(11, 'H'), Card(5, 'D'), Card(2, 'S'), Card(7, 'D'))
  val flushCards = List(Card(3, 'C'), Card(4, 'C'), Card(5, 'C'), Card(6, 'C'), Card(7, 'C'))

  "A line" should "be parsed correctly" in {
    val line = "2H 4S 5C 5S TC KC JD 6C TS 3C"
    val (s0, s1) = Problem54.parseLine(line)

    s0.cards should be(List(Card(2, 'H'), Card(4, 'S'), Card(5, 'C'), Card(5, 'S'), Card(10, 'C')))
    s1.cards should be(List(Card(13, 'C'), Card(11, 'D'), Card(6, 'C'), Card(10, 'S'), Card(3, 'C')))
  }

  "A hand" should "always have 5 cards" in {
    Hand(sampleCards)
    a[AssertionError] should be thrownBy {
      Hand(sampleCards.tail)
    }
    a[AssertionError] should be thrownBy {
      Hand(sampleCards(0) :: sampleCards)
    }
  }

  it should "be a flush if all cards are of the same suit" in {
    Hand(flushCards).flush should be(true)
    Hand(sampleCards).flush should be(false)
  }

  it should "be a straight if the card values are consecutive" in {
    Hand(flushCards).straight should be(true)
    Hand(sampleCards).straight should be(false)
  }

  it should "be a Royal Flush if it consists of AKQJT in the same suit" in {
    val royalFlush = List(Card(14, 'C'), Card(13, 'C'), Card(12, 'C'), Card(11, 'C'), Card(10, 'C'))
    val notRoyalFlush = List(Card(13, 'C'), Card(12, 'C'), Card(11, 'C'), Card(10, 'C'), Card(9, 'C'))

    Hand(royalFlush).ranking(0) should be(RoyalFlush)
    Hand(notRoyalFlush).ranking(0) shouldNot be(RoyalFlush)
  }

}

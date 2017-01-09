import org.scalatest.{FlatSpec, Matchers}
import poker._

/**
  * Created by luke on 1/4/17.
  */
class pokerTest extends FlatSpec with Matchers {

  val highCard = Card(Ace, Spades)
  val midCard = Card(Ace, Hearts)
  val lowCard = Card(King, Diamonds)
  val flushCard = Card(King, Spades)
  val jack = Card(Jack, Hearts)
  val queen = Card(Queen, Diamonds)
  val ten = Card(Ten, Clubs)

  val sTen = Card(Ten, Spades)
  val sJack = Card(Jack, Spades)
  val sQueen = Card(Queen, Spades)


  val f = List(flushCard, flushCard, highCard, highCard, highCard)
  val t = List(highCard, highCard, highCard)
  val d = List(lowCard, lowCard)
  val st = List(highCard, lowCard, ten, queen, jack)

  val stFl = List(sQueen, highCard, sJack, flushCard, sTen)

  val deck = poker.createDeck()
  val card = poker.popCard(deck)
  val hand = poker.dealHand(deck)
  val hc = poker.checkHighCard(hand)

  it should "return valid Deck: 52 cards, for each suit, one of each rank" in {
    isValidDeck(deck) shouldEqual true
  }

  it should "return a valid hand from deck: 5 cards, no duplicates" in {
    isValidHand(hand) shouldEqual true
  }

  it should "return a a sorted hand from deck: 5 cards sorted" in {
    sortHand(List.empty) shouldEqual List.empty
    //sortHand(hand)
    sortHand(List(highCard, flushCard, lowCard)) shouldEqual List(flushCard, lowCard, highCard)
  }

  it should "return Highcard[Ace, Spades] from hand" in {
    checkHighCard(List(lowCard, highCard)) shouldEqual handType(HighCard, Ace)
  }

  it should "return pair of cards from hand" in {
    checkPair(List(highCard, lowCard)) shouldEqual handType(Nothing, None)
    checkPair(List(highCard, midCard)) shouldEqual handType(Pair, Ace)
  }

  it should "return two pairs of cards from hand" in {
    checkTwoPair(List.empty) shouldEqual handType(Nothing, None)
    checkTwoPair(List(highCard, highCard, lowCard, lowCard)) shouldEqual handType(TwoPair,List(handType(Pair,King), handType(Pair, Ace)))
  }

  it should "return three cards of a kind from hand" in {
    checkThreeOfAKind(List.empty) shouldEqual handType(Nothing,None)
    checkThreeOfAKind(List(highCard, highCard, highCard)) shouldEqual handType(ThreeOfAKind, Ace)
  }

  it should "return four cards of a kind from hand" in {
    checkFourOfAKind(List.empty) shouldEqual handType(Nothing,None)
    checkFourOfAKind(List(highCard, highCard, highCard, highCard)) shouldEqual handType(FourOfAKind, Ace)
  }

  it should "return a fullhouse from a hand" in {
    checkFullHouse(List.empty) shouldEqual handType(Nothing,None)
    checkFullHouse(List(highCard, highCard, highCard, lowCard, lowCard)) shouldEqual handType(FullHouse, List[handType](checkThreeOfAKind(t), checkPair(d)))
  }

  it should "return a flush from a hand" in {
    checkFlush(List.empty) shouldEqual handType(Nothing, None)
    checkFlush(f) shouldEqual handType(Flush, Spades)
  }

  it should "return a straight from a hand" in {
    checkStraight(List.empty) shouldEqual handType(Nothing, None)
    checkStraight(hand)
    checkStraight(st) shouldEqual handType(Straight, sortHand(st))
  }

  it should "return a straight flush from a hand" in {
    checkStraightFlush(List.empty) shouldEqual handType(Nothing,None)
    checkStraightFlush(stFl) shouldEqual handType(StraightFlush, sortHand(stFl))
    checkStraightFlush(hand) shouldEqual handType(Nothing,None)
  }

  it should "return royal straight flush from a hand" in {
    checkRoyalStraightFlush(List.empty) shouldEqual handType(Nothing,None)
    checkRoyalStraightFlush(stFl) shouldEqual handType(RoyalStraightFlush, sortHand(stFl))
  }


}

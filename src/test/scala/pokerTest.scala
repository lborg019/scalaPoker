import org.scalatest.{FlatSpec, Matchers}
import poker._

/**
  * Created by luke on 1/4/17.
  */
class pokerTest extends FlatSpec with Matchers {

  val highCard = Card(poker.Ace, poker.Spades)
  val midCard = Card(poker.Ace, poker.Hearts)
  val lowCard = Card(poker.King, poker.Diamonds)

  val deck = poker.createDeck()
  val card = poker.popCard(deck)
  val hand = poker.dealHand(deck)
  val hc = poker.checkHighCard(hand)

  it should "return valid Deck: 52 cards, for each suit, one of each rank" in {
    poker.isValidDeck(deck) shouldEqual true
  }

  it should "return a valid hand from deck: 5 cards, no duplicates" in {
    poker.isValidHand(hand) shouldEqual true
  }

  it should "return Highcard[Ace, Spades] from hand" in {
    poker.checkHighCard(List(lowCard, highCard)) shouldEqual handType(HighCard, Ace)
  }

  it should "return pair of cards from hand" in {
    poker.checkPair(List(highCard, lowCard)) shouldEqual handType(Nothing, None)
    poker.checkPair(List(highCard, midCard)) shouldEqual handType(Pair, Ace)
  }

  it should "return two pairs of cards from hand" in {
    poker.checkTwoPairs(List.empty) shouldEqual handType(Nothing, None)
    poker.checkTwoPairs(List(highCard, highCard, lowCard, lowCard)) shouldEqual handType(TwoPairs,List(handType(Pair,King), handType(Pair, Ace)))
  }

  it should "return three cards of a kind from hand" in {
    poker.checkThreeOfAKind(List.empty) shouldEqual handType(Nothing,None)
    poker.checkThreeOfAKind(List(highCard, highCard, highCard)) shouldEqual handType(ThreeOfAKind, Ace)
  }

  it should "return four cards of a kind from hand" in {
    poker.checkFourOfAKind(List.empty) shouldEqual handType(Nothing,None)
    poker.checkFourOfAKind(List(highCard, highCard, highCard, highCard)) shouldEqual handType(FourOfAKind, Ace)
  }




}

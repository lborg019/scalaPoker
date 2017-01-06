/**
  * Created by luke on 1/4/17.
  */
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer
import scala.util.Random

object poker {

  //def main(args: Array[String]): Unit = {createDeck()}

  abstract class Suit
  case object Spades extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit
  case object Hearts extends Suit

  abstract class Rank
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

  var top = 0

  val suits = List(Spades, Diamonds, Clubs, Hearts)

  val ranks = List(Two, Three, Four, Five,
                   Six, Seven, Eight, Nine,
                   Ten, Jack, Queen, King, Ace)

  case class Card(rank: Rank, suit: Suit)

  case class Deck(cards: List[Card])

  case class Kit(deck: Deck, card: Card)

  def createDeck(): Deck = {

    val cds: List[Card] = for (
      r <- ranks;
      s <- suits
    ) yield Card(r,s)

    val dck = new Deck(shuffle(cds))
    dck
  }

  def shuffle(l: List[Card]): List[Card] = {
    top = 0
    Random.shuffle(l)
  }

  def isValidDeck(d: Deck): Boolean = {
    if (d.cards.length != d.cards.toSet.size|| d.cards.length != 52)
      false
    else true
  }

  def isValidHand(l: List[Card]): Boolean = {
    if (l.length != l.toSet.size|| l.length != 5)
      false
    else true
  }

  def popCard(d: Deck): Card = {
    val c = d.cards(top)
    top += 1
    c
  }

  def dealHand(d: Deck): List[Card] = {
    val hand: IndexedSeq[Card] = for(_ <- 0 to 4) yield popCard(d)
    println(hand.toList)
    hand.toList
  }

  def checkHighCard(x: List[Card]): Card = {
    var indexList= new ListBuffer[Int]()

    for (e <- x) {
      for (f <- ranks) {
        if (e.rank == f) { indexList += ranks.indexOf(f) }
      }
    }

    val maxIndexPos = indexList.zipWithIndex.maxBy(_._1)._2
    val maxIndexVal = indexList(maxIndexPos)
    val highCardRank = ranks(maxIndexVal)
    val highestCard = x.filter(_.rank == highCardRank)

    //println("highcard: "+highestCard(0))
    highestCard(0)
  }

  def checkPair(x: List[Card]): List[Card] = {
    val p = x.groupBy(_.rank).map(_._2.head)

    if(p.isEmpty)
      List.empty
    else
      println("pair check:" + p.toList)
      p.toList

  }

  def checkTwoPairs() = {

  }

  //royal straight flush (straight flush Ten to Ace)
  //straight flush
  //four
  //full house
  //flush
  //straight
  //three of a kind
  //two pair
  //pair
  //high card
}

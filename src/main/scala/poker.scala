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

  abstract class Combination
  case object Nothing extends Combination
  case object HighCard extends Combination
  case object Pair extends Combination
  case object TwoPair extends Combination
  case object ThreeOfAKind extends Combination
  case object Straight extends Combination
  case object Flush extends Combination
  case object FullHouse extends Combination
  case object FourOfAKind extends Combination
  case object StraightFlush extends Combination
  case object RoyalStraightFlush extends Combination

  var top = 0

  val suits = List(Spades, Diamonds, Clubs, Hearts)

  val ranks = List(Two, Three, Four, Five,
                   Six, Seven, Eight, Nine,
                   Ten, Jack, Queen, King, Ace)

  val handCombo = List(HighCard, Pair, TwoPair, ThreeOfAKind,
                      FullHouse, Straight, Flush, FullHouse,
                      FourOfAKind, StraightFlush, RoyalStraightFlush)

  case class Card(rank: Rank, suit: Suit)

  case class Deck(cards: List[Card])

  case class handType(combination: Combination, any: Any){
    def apply(combination: Combination) = new handType(poker.Nothing, None)
  }

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

  def sortHand(h: List[Card]): List[Card] = {
    var indexList = new ListBuffer[Int]()
    var sortedRanks = new ListBuffer[Rank]()
    val entities: Seq[Card] = h

    //not taking duplicates into account
    //val idToEntityMap = entities.map(e => e.rank -> e).toMap

    //taking duplicate cards into account
    val group = entities.groupBy(_.rank)
    val idToEntityMap = group.map(e => e._1 -> e._2)
    //println("idEntityMap: "+idToEntityMap)

    if(h.isEmpty){
      val empty = List.empty
      empty

    }
    else {
      //convert to integers
      for (e <- h) {
        for (f <- ranks) {
          if (e.rank == f)
            indexList += ranks.indexOf(f)
        }
      }
      //sort with integer comparison and convert to rank
      indexList = indexList.sortWith(_ < _)
      for (e <- indexList) {
        sortedRanks += ranks(e)
      }
      val sorted = sortedRanks.map(idToEntityMap)
      val s = sorted.distinct.flatten.toList
      //println(s)
      s
    }
  }

  def checkHighCard(x: List[Card]): handType = {
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
    handType(HighCard, highestCard(0).rank)
  }

  def checkPair(x: List[Card]): handType = {
    val p = x.groupBy(_.rank) //p: [_.rank, List(CardsWithThatRank)]

    p foreach (tuple =>
      if (tuple._2.length == 2) {
        val filtered = x.filter(_.rank == tuple._1)
        val r = handType(Pair, filtered(0).rank)
        //println("pair: "+r)
        return r
      })

    val empty = handType(poker.Nothing, None)
    empty
  }

  def checkTwoPair(x: List[Card]): handType = {
    val p = x.groupBy(_.rank) //p: [_.rank, List(CardsWithThatRank)]
    var l = new ListBuffer[handType]()
    p foreach (tuple =>
      if (tuple._2.length == 2) {
        val filtered = x.filter(_.rank == tuple._1)
        val r = handType(Pair, filtered(0).rank)
        l += r
      })

    if(l.toList.size < 2) //less than two pairs
      handType(Nothing, None)
    else {
      //println("twoPairs: " + l.toList)
      handType(TwoPair, l.toList)
    }
  }

  def checkThreeOfAKind(x: List[Card]): handType = {
    val p = x.groupBy(_.rank) //p: [_.rank, List(CardsWithThatRank)]

    p foreach (tuple =>
      if (tuple._2.length == 3) {
        val filtered = x.filter(_.rank == tuple._1)
        val r = handType(ThreeOfAKind, filtered(0).rank)
        //println("trip: "+r)
        return r
      })

    val empty = handType(poker.Nothing, None)
    empty
  }

  def checkFourOfAKind(x: List[Card]): handType = {
    val p = x.groupBy(_.rank) //p: [_.rank, List(CardsWithThatRank)]

    p foreach (tuple =>
      if (tuple._2.length == 4) {
        val filtered = x.filter(_.rank == tuple._1)
        val r = handType(FourOfAKind, filtered(0).rank)
        //println("four: "+r)
        return r
      })

    val empty = handType(poker.Nothing, None)
    empty
  }

  def checkFullHouse(x: List[Card]): handType = {
    val pair: handType = checkPair(x)
    val trip: handType = checkThreeOfAKind(x)

    if(pair.combination != Nothing && trip.combination != Nothing)
    {
      val result = handType(FullHouse, List[handType](trip, pair))
      //println("fullhouse: "+result)
      return result
    }

    val empty = handType(Nothing, None)
    empty
  }

  def checkFlush(x: List[Card]): handType = {
    val f = x.groupBy(_.suit) //p: [_.suit, List(CardsWithThatSuit)]
    f foreach (tuple =>
      if (tuple._2.length == 5) {
        val filtered = x.filter(_.suit == tuple._1)
        val r = handType(Flush, filtered(0).suit)
        //println("flush: "+r)
        return r
      })

    val empty = handType(Nothing, None)
    empty
  }

  def checkStraight(h: List[Card]): handType = {
    var indexList = new ListBuffer[Int]()
    val x = sortHand(h)
    val empty = handType(Nothing, None)

    if(x.isEmpty) {
      empty
    }else{
      //convert to integers
      for (e <- x) {
        for (f <- ranks) {
          if (e.rank == f)
            indexList += ranks.indexOf(f)
        }
      }

      val diffs = indexList.toList.sliding(2).map{case List(x, y) => x - y }.sum
      if(diffs != -4)
        return empty
      else {
        val result = handType(Straight, x)
        return result
      }
    }
  }

  def checkStraightFlush(h: List[Card]): handType = {
    val chStr = checkStraight(h)
    val chFl = checkFlush(h)

    if(chStr.combination == Straight && chFl.combination == Flush) {
      val result = handType(StraightFlush, sortHand(h))
      println(result)
      return result
    }
    else
      return handType(Nothing,None)
  }

  def checkRoyalStraightFlush(h: List[Card]): handType = {
    val e = handType(Nothing,None)
    val checkOne = checkStraight(h)
    val checkTwo = checkFlush(h)

    if(checkOne.combination == Straight && checkTwo.combination == Flush){
      val checkThree = checkHighCard(h)
      if(checkThree.any == Ace){
        println(handType(RoyalStraightFlush, sortHand(h)))
        return handType(RoyalStraightFlush, sortHand(h))
      } else
        return e
    }
    else
      return e
  }
}

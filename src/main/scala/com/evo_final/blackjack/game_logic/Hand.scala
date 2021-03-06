package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Rank.Ace
import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic.adt.HandResult._
import com.evo_final.blackjack.game_logic.adt.HandResult

import scala.annotation.tailrec

case class Hand(cards: List[Card]) {

  @tailrec
  private def considerAces(value: Int, aceCount: Int): Int = {
    if (value > 21 && aceCount > 0)
      considerAces(value - 10, aceCount - 1)
    else
      value
  }
  def score: Int = {
    val baseValue = cards.foldLeft(0)((acc, v) => acc + v.rank.strength)

    considerAces(baseValue, cards.count(_.rank == Ace))
  }
  def isBlackJack: Boolean = score == 21 && cards.size == 2
  def isBust: Boolean = score > 21

  def getServed(n: Int, deck: GameDeck): (Hand, GameDeck) = {
    val (servedCards, newDeck) = deck.serveN(n)
    (Hand(servedCards ++ cards), newDeck)
  }

  def handResult(hand: Hand): HandResult = {
    if (!this.isBust && !hand.isBust)
      Integer.compare(this.score, hand.score) match {
        case 1 => Won
        case 0 =>
          if (this.isBlackJack == hand.isBlackJack) Tie
          else if (this.isBlackJack) Won
          else Lost
        case -1 => Lost
      }
    else if (hand.isBust) Won
    else Lost
  }
}

object Hand {
  def empty: Hand = Hand(List())
}

package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Rank.Ace
import com.evo_final.blackjack.cards.Card

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
}

object Hand {
  def empty: Hand = Hand(List())
}

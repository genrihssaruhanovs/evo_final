package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.ErrorMessage
import com.evo_final.blackjack.cards.Rank.Ace
import com.evo_final.blackjack.cards.Card

import scala.annotation.tailrec

sealed trait Hand[H <: Hand[H]] {
  val cards: List[Card]

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

}

case class PlayerHand(cards: List[Card]) extends Hand[PlayerHand] {}
case class DealerHand(cards: List[Card]) extends Hand[DealerHand] {}

object PlayerHand {
  def of(cards: List[Card]): Either[ErrorMessage, PlayerHand] = {
    if (cards.size >= 2)
      Right(PlayerHand(cards))
    else
      Left("Number of initial cards is incorrect")
  }

  def empty: PlayerHand = PlayerHand(List())
}

object DealerHand {
  def of(cards: List[Card]): Either[ErrorMessage, DealerHand] = {
    if (cards.nonEmpty)
      Right(DealerHand(cards))
    else
      Left("No initial cards provided")
  }

  def empty: DealerHand = DealerHand(List())
}

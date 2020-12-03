package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.adt.{Ace, Card}

import scala.annotation.tailrec

case class PlayerHand(cards: List[Card]) {

  @tailrec
  private def considerAces(value: Int, aceCount: Int): Int = {
    if (value > 21 && aceCount > 0)
      considerAces(value - 10, aceCount - 1)
    else
      value
  }

  def evaluate: Int = {
    val baseValue = cards.foldLeft(0)((acc, v) => acc + v.rank.strength)

    considerAces(baseValue, cards.count(_.rank == Ace))
  }
}

object PlayerHand {
  def of(cards: List[Card]): Option[PlayerHand] = {
    if (cards.size >= 2)
      Some(PlayerHand(cards))
    else
      None
  }
}

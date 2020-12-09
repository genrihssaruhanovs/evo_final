package com.evo_final.blackjack.cards

final case class Card(
  rank: Rank,
  suit: Suit,
) {
  override def toString: String = s"$rank$suit"
}

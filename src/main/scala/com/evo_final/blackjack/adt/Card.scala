package com.evo_final.blackjack.adt

final case class Card(
  rank: Rank,
  suit: Suit,
) {
  override def toString: String = s"$rank$suit"
}

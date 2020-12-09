package com.evo_final.blackjack.cards

object Deck {
  def get: Set[Card] =
    (for {
      rank <- Rank.allRanks
      suit <- Suit.allSuits
    } yield Card(rank, suit)).toSet
}

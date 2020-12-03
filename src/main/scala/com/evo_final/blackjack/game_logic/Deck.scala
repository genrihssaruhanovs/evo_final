package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.adt.{Card, Rank, Suit}
import com.evo_final.blackjack.adt

import scala.util.Random

case class GameDeck(cards: List[Card]) {
  def shuffle: GameDeck = GameDeck(Random.shuffle(cards))
  def serve: Option[(Card, GameDeck)] = {
    cards match {
      case x :: xs => Some((x, GameDeck(xs)))
      case _       => None
    }
  }
}

object GameDeck {
  private def nDecks(n: Int): List[Card] =
    for {
      card <- Deck.get.toList
      _    <- 1 to n
    } yield card

  def of(deckCount: Int): GameDeck = GameDeck(nDecks(deckCount)).shuffle
}

private object Deck {
  def get: Set[Card] =
    (for {
      rank <- Rank.allRanks
      suit <- Suit.allSuits
    } yield adt.Card(rank, suit)).toSet
}

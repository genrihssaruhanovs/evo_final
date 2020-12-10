package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.{Card, Deck}

import scala.annotation.tailrec
import scala.util.Random

case class GameDeck(cards: List[Card]) {
  def shuffle: GameDeck = GameDeck(Random.shuffle(cards))

  private def serve: (Card, GameDeck) = {
    (cards.head, GameDeck(cards.tail))
  }

  def serveN(n: Int): (List[Card], GameDeck) = {

    @tailrec
    def getServed(
      n: Int,
      servedCards: List[Card],
      deck: GameDeck
    ): (List[Card], GameDeck) = {
      if (servedCards.size < n) {
        val (card, newDeck) = deck.serve
        getServed(n, card :: servedCards, newDeck)
      } else {
        (servedCards, deck)
      }
    }

    getServed(n, List(), this)
  }
}

object GameDeck {
  private def nDecks(n: Int): List[Card] =
    for {
      card <- Deck.get.toList
      _    <- 1 to n
    } yield card

  def of(deckCount: Int): GameDeck = GameDeck(nDecks(deckCount)).shuffle
  def empty: GameDeck = GameDeck(List())
}

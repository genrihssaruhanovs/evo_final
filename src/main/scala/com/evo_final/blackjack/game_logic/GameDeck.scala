package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.ErrorMessage
import com.evo_final.blackjack.cards.{Card, Deck}

import scala.util.Random

case class GameDeck(cards: List[Card]) {
  def shuffle: GameDeck = GameDeck(Random.shuffle(cards))
  private def serve: Either[ErrorMessage, (Card, GameDeck)] = {
    cards match {
      case x :: xs => Right((x, GameDeck(xs)))
      case Nil     => Left("Not enough cards in the deck")
    }
  }

  def serveN(n: Int): Either[ErrorMessage, (List[Card], GameDeck)] = {

    def getServed(
      n: Int,
      served: List[Card],
      deck: GameDeck
    ): Either[ErrorMessage, (List[Card], GameDeck)] = {
      if (served.size < n) {
        deck.serve.flatMap {
          case (card, newDeck) => getServed(n, card :: served, newDeck)
        }
      } else {
        Right((served, deck))
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

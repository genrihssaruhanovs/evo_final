package com.evo_final.blackjack.game_logic

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GameDeckSpec extends AnyFreeSpec with Matchers {
  "Game deck should" - {
    "build number of decks provided" in {
      GameDeck.of(1).cards should have size 52
      GameDeck.of(4).cards should have size 208
      GameDeck.of(5).cards should have size 260
    }

    "shuffle cards" in {
      val deck = GameDeck.of(4)
      deck.cards.head should not equal deck.shuffle.cards.head
      deck.shuffle.cards.head should not equal deck.shuffle.cards.head
    }

    "serve one card" in {
      val deck = GameDeck.of(4)
      val served = deck.serveN(1)

      val (servedCards, resultDeck) = served.getOrElse(List(), GameDeck.empty)
      resultDeck.cards.size shouldEqual deck.cards.size - 1
      servedCards should have size 1
    }

    "serve 2 cards" in {
      val deck = GameDeck.of(4)
      val served = deck.serveN(2)

      val (servedCards, resultDeck) = served.getOrElse(List(), GameDeck.empty)

      resultDeck.cards should have size deck.cards.size - 2
      servedCards should have size 2
    }
  }
}

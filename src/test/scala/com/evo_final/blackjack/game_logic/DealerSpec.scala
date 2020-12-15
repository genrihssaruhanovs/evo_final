package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.cards.Rank.{Ace, Eight, Four, King, Queen, Three}
import com.evo_final.blackjack.cards.Suit.Hearts
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DealerSpec extends AnyFreeSpec with Matchers {

  "Dealer should" - {

    val fakeDeck = GameDeck(
      List(
        Card(Queen, Hearts),
        Card(Three, Hearts),
        Card(Ace, Hearts),
        Card(Four, Hearts),
        Card(Eight, Hearts),
        Card(King, Hearts),
        Card(Ace, Hearts),
      )
    )

    "Be initially served with one card" in {
      val (servedDealer, resultDeck) = Dealer.of(fakeDeck)

      servedDealer.hand.cards should have size 1
      servedDealer.hand.cards shouldEqual List(Card(Queen, Hearts))
      servedDealer.hand.score shouldEqual 10
      resultDeck.cards should have size fakeDeck.cards.size - 1
    }

    "Take cards until exceeds score 17" in {
      val (servedDealer, resultDeck) = Dealer.of(fakeDeck)

      val (playedDealer, playedDeck) = servedDealer.play(resultDeck)

      playedDealer.hand.cards should have size 4
      playedDealer.hand.score shouldEqual 18
      playedDeck.cards should have size fakeDeck.cards.size - 4
    }
  }

}

package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.cards.Rank._
import com.evo_final.blackjack.cards.Suit._
import com.evo_final.blackjack.game_logic.PlayerDecision._
import com.evo_final.blackjack.game_logic.PlayerState._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BoardMemberSpec extends AnyFreeSpec with Matchers {

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

  "Player should" - {
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

    val fakeDealer = Dealer(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds))))

    "evaluate hand correctly" in {
      val player1 = Player(Hand(List(Card(Ace, Hearts), Card(Queen, Hearts))), Set(), 500)
      val player2 = Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts))), Set(), 500)
      val player3 =
        Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts), Card(Seven, Hearts))), Set(), 500)
      val player4 = Player(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds))), Set(), 500)

      player1.evaluate(fakeDealer.hand) shouldEqual 1250
      player2.evaluate(fakeDealer.hand) shouldEqual 0
      player3.evaluate(fakeDealer.hand) shouldEqual 1000
      player4.evaluate(fakeDealer.hand) shouldEqual 500
    }

    "Execute different actions" - {
      val player = Player(Hand(List(Card(Five, Hearts), Card(Four, Hearts))), Set(TurnNow), 500)

      "Execute stand action" in {

        val stood = player.makeDecision(Stand, fakeDeck)
        val (stoodPlayer, stoodDeck) = stood.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        stoodDeck.cards shouldEqual fakeDeck.cards
        stoodPlayer.states should contain(TurnDone)
        stoodPlayer.states should not contain TurnNow
        stoodPlayer.hand.score shouldEqual player.hand.score
      }

      "Execute hit action" in {

        val hit = player.makeDecision(Hit, fakeDeck)
        val (hitPlayer, hitDeck) = hit.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        hitDeck.cards shouldEqual fakeDeck.cards.tail
        hitPlayer.states should contain(TurnNow)
        hitPlayer.states should not contain TurnDone
        hitPlayer.hand.score shouldEqual 19

        val hit2 = hitPlayer.makeDecision(Hit, hitDeck)
        val (hitPlayer2, hitDeck2) = hit2.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        hitDeck2.cards shouldEqual hitDeck.cards.tail
        hitPlayer2.states should contain(TurnDone)
        hitPlayer2.states should not contain TurnNow
        hitPlayer2.hand.score shouldEqual 22
      }
      "Execute double down action" in {

        val doubleDowned = player.makeDecision(DoubleDown, fakeDeck)
        val (doubleDownPlayer, doubleDownDeck) =
          doubleDowned.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        doubleDownDeck.cards shouldEqual fakeDeck.cards.tail
        doubleDownPlayer.states should contain(TurnDone)
        doubleDownPlayer.states should not contain TurnNow
        doubleDownPlayer.hand.score shouldEqual 19
        doubleDownPlayer.bet shouldEqual player.bet * 2
      }

      "Execute surrender action" in {

        val surrendered = player.makeDecision(Surrender, fakeDeck)
        val (surrenderedPlayer, surrenderedDeck) =
          surrendered.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        surrenderedDeck.cards shouldEqual fakeDeck.cards
        surrenderedPlayer.states should contain(TurnDone)
        surrenderedPlayer.states should not contain TurnNow
        surrenderedPlayer.states should contain(Surrendered)
        surrenderedPlayer.hand.score shouldEqual player.hand.score
      }

      "Execute insure action" in {

        val insured = player.makeDecision(Insurance, fakeDeck)
        val (insuredPlayer, insuredDeck) =
          insured.getOrElse(Player(Hand.empty, Set(), 0), GameDeck.empty)

        insuredDeck.cards shouldEqual fakeDeck.cards
        insuredPlayer.states should contain(TurnDone)
        insuredPlayer.states should not contain TurnNow
        insuredPlayer.states should contain(Insured)
        insuredPlayer.hand.score shouldEqual player.hand.score
      }
    }
  }
}

package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.cards.Rank._
import com.evo_final.blackjack.cards.Suit._
import com.evo_final.blackjack.game_logic.adt.PlayerDecision._
import com.evo_final.blackjack.game_logic.adt.PlayerState._
import com.evo_final.blackjack.game_logic.adt.PossibleActions._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PlayerSpec extends AnyFreeSpec with Matchers {

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

    "evaluate hand correctly vs non-blackjack dealer" in {
      val fakeDealer = Dealer(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds))))
      val player1 = Player(Hand(List(Card(Ace, Hearts), Card(Queen, Hearts))), Set())
      val player2 = Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts))), Set())
      val player3 =
        Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts), Card(Seven, Hearts))), Set())
      val player4 = Player(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds))), Set())
      val player5 =
        Player(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds), Card(Ten, Hearts))), Set())

      player1.evaluate(fakeDealer.hand) shouldEqual 2.5
      player2.evaluate(fakeDealer.hand) shouldEqual 0
      player3.evaluate(fakeDealer.hand) shouldEqual 2
      player4.evaluate(fakeDealer.hand) shouldEqual 1
      player5.evaluate(fakeDealer.hand) shouldEqual 0
    }

    "evaluate hand correctly vs blackjack dealer" in {
      val fakeDealer = Dealer(Hand(List(Card(Ten, Diamonds), Card(Ace, Diamonds))))
      val player1 = Player(Hand(List(Card(Ace, Hearts), Card(Queen, Hearts))), Set())
      val player2 = Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts))), Set())
      val player3 =
        Player(Hand(List(Card(Ace, Hearts), Card(Three, Hearts), Card(Seven, Hearts))), Set())
      val player4 = Player(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds))), Set())
      val player5 =
        Player(Hand(List(Card(Ten, Diamonds), Card(Nine, Diamonds), Card(Ten, Hearts))), Set())

      player1.evaluate(fakeDealer.hand) shouldEqual 1
      player2.evaluate(fakeDealer.hand) shouldEqual 0
      player3.evaluate(fakeDealer.hand) shouldEqual 0
      player4.evaluate(fakeDealer.hand) shouldEqual 0
      player5.evaluate(fakeDealer.hand) shouldEqual 0
    }
    "Execute different actions" - {
      val player = Player(Hand(List(Card(Five, Hearts), Card(Four, Hearts))), Set(TurnNow))

      "Execute stand action" in {

        val stood = player.makeDecision(Stand, fakeDeck)
        val (stoodPlayer, stoodDeck) = stood

        stoodDeck.cards shouldEqual fakeDeck.cards
        stoodPlayer.states should contain(TurnDone)
        stoodPlayer.states should not contain TurnNow
        stoodPlayer.hand.score shouldEqual player.hand.score
      }

      "Execute hit action" in {

        val hit = player.makeDecision(Hit, fakeDeck)
        val (hitPlayer, hitDeck) = hit

        hitDeck.cards shouldEqual fakeDeck.cards.tail
        hitPlayer.states should contain(TurnNow)
        hitPlayer.states should not contain TurnDone
        hitPlayer.hand.score shouldEqual 19

        val hit2 = hitPlayer.makeDecision(Hit, hitDeck)
        val (hitPlayer2, hitDeck2) = hit2

        hitDeck2.cards shouldEqual hitDeck.cards.tail
        hitPlayer2.states should contain(TurnDone)
        hitPlayer2.states should not contain TurnNow
        hitPlayer2.hand.score shouldEqual 22
      }
      "Execute double down action" in {

        val doubleDowned = player.makeDecision(DoubleDown, fakeDeck)
        val (doubleDownPlayer, doubleDownDeck) =
          doubleDowned

        doubleDownDeck.cards shouldEqual fakeDeck.cards.tail
        doubleDownPlayer.states should contain(TurnDone)
        doubleDownPlayer.states should contain(DoubleDowned)
        doubleDownPlayer.states should not contain TurnNow
        doubleDownPlayer.hand.score shouldEqual 19
      }

      "Execute surrender action" in {

        val surrendered = player.makeDecision(Surrender, fakeDeck)
        val (surrenderedPlayer, surrenderedDeck) =
          surrendered

        surrenderedDeck.cards shouldEqual fakeDeck.cards
        surrenderedPlayer.states should contain(TurnDone)
        surrenderedPlayer.states should not contain TurnNow
        surrenderedPlayer.states should contain(Surrendered)
        surrenderedPlayer.hand.score shouldEqual player.hand.score

        surrenderedPlayer.evaluate(Hand(List(Card(Ace, Spades)))) shouldEqual 0.5
      }

      "Execute insure action" in {

        val insured = player.makeDecision(Insurance, fakeDeck)
        val (insuredPlayer, insuredDeck) =
          insured

        insuredDeck.cards shouldEqual fakeDeck.cards
        insuredPlayer.states should not contain TurnDone
        insuredPlayer.states should contain(TurnNow)
        insuredPlayer.states should contain(Insured)
        insuredPlayer.hand.score shouldEqual player.hand.score
      }
    }
    "Return correct possible actions" in {
      val fakeDealer = Dealer(Hand(List(Card(Ace, Diamonds))))
      val player1 = Player(Hand(List(Card(Ten, Hearts), Card(Ten, Hearts))), Set(TurnNow))
      val possibleActions = player1.getPossibleActions(fakeDealer)
      possibleActions should contain(CanInsure)
      possibleActions should contain(CanSplit)
      possibleActions should contain(CanHit)
      possibleActions should contain(CanStand)
      possibleActions should contain(CanSurrender)
    }
  }
}

package com.evo_final.blackjack.game_logic

import java.util.UUID

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.cards.Rank._
import com.evo_final.blackjack.cards.Suit.Hearts
import com.evo_final.blackjack.game_logic.adt.PlayerDecision._
import com.evo_final.blackjack.game_logic.adt.PlayerState._
import com.evo_final.blackjack.game_logic.adt.PossibleActions._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RoundSpec extends AnyFreeSpec with Matchers {
  "Round should" - {
    val playerIdList = List(
      "2951bf2c-e47b-46eb-b665-0773f8ccf0f2",
      "2951bf2c-e47b-46eb-b665-0773f8ccf0f3",
      "2951bf2c-e47b-46eb-b665-0773f8ccf0f4"
    ).map(UUID.fromString)

    "Start properly" - {
      val startedRound = Round.start(playerIdList)
      "Players should be served" in {
        //players
        startedRound.players should have size 3
        startedRound.players.foreach {
          case (_, player) => player.hand.cards should have size 2
        }
      }

      "One player should have turn" in {
        val turnNowCount = startedRound.players.foldLeft(0)((acc, playerMap) => {
          val (_, player) = playerMap
          if (player.states.contains(TurnNow)) acc + 1 else acc
        })

        turnNowCount shouldEqual 1
      }

      "Dealer should be served" in {
        startedRound.dealer.hand.cards should have size 1
      }

      "Deck should have correct amount of cards" in {
        startedRound.deck.cards should have size 52 * 4 - playerIdList.size * 2 - 1 //52 * deck count - served cards
      }
    }

    "Process requests" - {
      val fakeDeck = GameDeck(
        List(
          Card(Queen, Hearts),
          Card(King, Hearts),
          Card(Ace, Hearts),
          Card(Four, Hearts),
          Card(Eight, Hearts),
          Card(King, Hearts),
          Card(Ace, Hearts),
          Card(Ace, Hearts),
          Card(Four, Hearts)
        )
      )
      val playerList = (playerIdList zip List(
        Player(Hand(List(Card(Ace, Hearts), Card(Queen, Hearts))), Set(TurnNow)),
        Player(Hand(List(Card(King, Hearts), Card(Three, Hearts))), Set())
      )).toMap

      val dealer = Dealer(Hand(List(Card(Ace, Hearts))))
      val round = Round(playerList, dealer, fakeDeck)

      "Execute Decisions" in {
        val roundIt1 = round.runDecision(playerIdList.head, Stand).get

        roundIt1.players(playerIdList.head).hand.cards should have size 2
        roundIt1.players(playerIdList.head).states should contain(TurnDone)

        val roundIt2 = roundIt1.runDecision(playerIdList(1), Hit).get
        val secondPlayer = roundIt2.players(playerIdList(1))
        secondPlayer.hand.cards should have size 3
        secondPlayer.states should contain(TurnDone)

        roundIt2.dealer.hand.cards should have size 2
      }

      "Pass turn" in {
        val roundIt1 = round.runDecision(playerIdList.head, Stand).get
        roundIt1.players(playerIdList.head).states should contain(TurnDone)
        roundIt1.players(playerIdList(1)).states should contain(TurnNow)

        val roundIt2 = roundIt1.runDecision(playerIdList(1), Stand).get
        roundIt2.players(playerIdList(1)).states should contain(TurnDone)
      }

      "Provide possible player actions" in {
        val possibleActionsP1 = round.getPossibleActions(playerIdList.head)

        possibleActionsP1 should contain(CanHit)
        possibleActionsP1 should contain(CanStand)
        possibleActionsP1 should contain(CanDoubleDown)
        possibleActionsP1 should contain(CanSurrender)

        val possibleActionsP2 = round.getPossibleActions(playerIdList(1))
        possibleActionsP2 shouldEqual Set()
      }

      "Provide winning coefficients" in {
        val winningCoefs = round.calculateWinningCoefficients

        winningCoefs.get(playerIdList.head) shouldEqual Some(2.5)
        winningCoefs.get(playerIdList(1)) shouldEqual Some(2)
      }
    }
  }
}

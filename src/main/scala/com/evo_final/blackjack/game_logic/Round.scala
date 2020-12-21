package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack._
import com.evo_final.blackjack.game_logic.adt.PlayerState._
import com.evo_final.blackjack.game_logic.adt.{PlayerDecision, PossibleActions}

import scala.annotation.tailrec

case class Round(
  players: Map[PlayerId, Player],
  dealer: Dealer,
  deck: GameDeck
) {

  def calculateWinningCoefficients: Map[PlayerId, Amount] = {
    players.map {
      case (id, player) => id -> player.evaluate(dealer.hand)
    }
  }

  def runDecision(id: PlayerId, decision: PlayerDecision): Option[Round] = {
    val roundAfterDecision = for {
      player <- players.get(id)
      if player.states.contains(TurnNow) && !player.states.contains(TurnDone)
      (updatedPlayer, newDeck) = player.makeDecision(decision, deck)
    } yield copy(players = players + (id -> updatedPlayer), deck = newDeck)

    roundAfterDecision.map(_.setNextTurn())
  }

  private def setNextTurn(): Round = {
    if (passTurnRequired) {
      players.find { case (_, player) => !player.states.contains(TurnDone) } match {
        case Some((playerId, player)) =>
          copy(players = players + (playerId -> player.copy(states = player.states + TurnNow)))
        case None => // if all players made their move - dealers turn
          val (newDealer, newDeck) = dealer.play(deck)
          copy(dealer = newDealer, deck = newDeck)
      }
    } else this
  }

  def getPossibleActions(id: PlayerId): Set[PossibleActions] = {

    players.get(id) match {
      case Some(player) =>
        player.getPossibleActions(dealer)
      case None => Set() // should never ever happen TODO
    }
  }

  private def passTurnRequired: Boolean =
    !players.exists { case (_, player) => player.states.contains(TurnNow) }
  def roundEnded: Boolean =
    !players.exists {
      case (_, player) => !player.states.contains(TurnDone)
    }
}

object Round {

  def start(players: List[PlayerId]): Round = {
    @tailrec
    def servePlayers(
      unservedPlayers: List[PlayerId],
      servedPlayers: Map[PlayerId, Player],
      currentDeck: GameDeck
    ): (Map[PlayerId, Player], GameDeck) = {
      unservedPlayers match {
        case x :: xs =>
          val (servedPlayer, newDeck) = Player.of(currentDeck)
          servePlayers(xs, servedPlayers + (x -> servedPlayer), newDeck)
        case Nil => (servedPlayers, currentDeck)
      }
    }

    val deckCount = Math.max(players.size / 2, 4)
    val deck = GameDeck.of(deckCount)
    val (servedPlayers, currentDeck) = servePlayers(players, Map.empty, deck)
    val (servedDealer, resultDeck) = Dealer.of(currentDeck)

    Round(servedPlayers, servedDealer, resultDeck).setNextTurn()
  }
}

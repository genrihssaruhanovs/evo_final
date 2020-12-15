package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack._
import com.evo_final.blackjack.game_logic.PlayerState._

import scala.annotation.tailrec

case class Round(
  players: Map[PlayerId, Player],
  dealer: Dealer,
  deck: GameDeck
) {

  def calculateWinnings: Map[PlayerId, Amount] = {
    players.map {
      case (id, player) => id -> player.evaluate(dealer.hand)
    }
  }

  def runDecision(id: PlayerId, decision: PlayerDecision): Option[Round] =
    for {
      player <- players.get(id)
      if player.states.contains(TurnNow) && !player.states.contains(TurnDone)
      (updatedPlayer, newDeck) = player.makeDecision(decision, deck)
    } yield copy(players = players + (id -> updatedPlayer), deck = newDeck)

  def setNextTurn(): Round = {
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

  def passTurnRequired: Boolean = !players.exists { case (_, player) => player.states.contains(TurnNow) }
  def roundEnded: Boolean =
    !players.exists {
      case (_, player) => !player.states.contains(TurnDone)
    }
}

object Round {

  def start(playerBets: Map[PlayerId, Amount]): Round = {
    @tailrec
    def servePlayers(
      unservedPlayers: List[(PlayerId, Amount)],
      servedPlayers: Map[PlayerId, Player],
      currentDeck: GameDeck
    ): (Map[PlayerId, Player], GameDeck) = {
      unservedPlayers match {
        case x :: xs =>
          val (id, bet) = x
          val (servedPlayer, newDeck) = Player.of(currentDeck, bet)
          servePlayers(xs, servedPlayers + (id -> servedPlayer), newDeck)
        case Nil => (servedPlayers, currentDeck)
      }
    }

    val deckCount = Math.max(playerBets.size / 2, 4)
    val deck = GameDeck.of(deckCount)
    val (servedPlayers, currentDeck) = servePlayers(playerBets.toList, Map.empty, deck)
    val (servedDealer, resultDeck) = Dealer.of(currentDeck)

    Round(servedPlayers, servedDealer, resultDeck).setNextTurn()
  }
}

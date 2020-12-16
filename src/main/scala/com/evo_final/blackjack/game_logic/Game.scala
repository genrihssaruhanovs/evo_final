package com.evo_final.blackjack.game_logic

import cats.implicits._
import com.evo_final.blackjack._
import com.evo_final.blackjack.game_logic.PossibleActions.CanPlaceBet

case class Game(
  connectedClients: Map[PlayerId, Option[Amount]],
  pendingClients: Set[PlayerId],
  roundOpt: Option[Round]
) {
  def startRound(): Game =
    copy(roundOpt = Some(Round.start(connectedClients.collect { case (id, Some(amount)) => id -> amount })))

  def resetRound(): Game = {
    roundOpt match {
      case Some(_) =>
        Game(
          connectedClients
            .map[PlayerId, Option[Amount]] { case (id, _) => id -> None }
            .combine(pendingClients.map(x => x -> None).toMap),
          Set(),
          None
        )

      case None => this //TODO
    }
  }

  def newPlayer(id: PlayerId): Game = {
    roundOpt match {
      case Some(_) => copy(pendingClients = pendingClients + id)
      case None    => copy(connectedClients = connectedClients + (id -> None))
    }
  }

  def playerBet(id: PlayerId, bet: Amount): Option[Game] = {
    connectedClients
      .get(id)
      .filter(_.isEmpty)
      .map(_ => {
        val updGame = copy(connectedClients + (id -> Some(bet)))
        if (updGame.allBetsPlaced) updGame.startRound() else updGame
      })
  }
  def allBetsPlaced: Boolean =
    !connectedClients.exists { case (_, amount) => amount.isEmpty }

  def roundEnded: Boolean =
    roundOpt match {
      case Some(round) => round.roundEnded
      case None        => false
    }

  def handleDecision(id: PlayerId, decision: PlayerDecision): Option[Game] = {
    for {
      round              <- roundOpt
      roundAfterDecision <- round.runDecision(id, decision)
    } yield copy(roundOpt = Some(roundAfterDecision.setNextTurn()))
  }

  def getWinnings: Option[Map[PlayerId, Amount]] = {
    roundOpt.map(_.calculateWinnings)
  }

  def getPossibleActions(id: PlayerId): Set[PossibleActions] = {
    connectedClients.get(id) match {
      case Some(bet) =>
        bet match {
          case Some(_) =>
            roundOpt match {
              case Some(round) => round.getPossibleActions(id)
              case None        => Set()
            }
          case None => Set(CanPlaceBet)
        }
      case None => Set()
    }
  }
}

object Game {
  def start: Game = Game(Map(), Set(), None)
}

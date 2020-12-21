package com.evo_final.blackjack.game_logic

import cats.implicits._
import com.evo_final.blackjack._
import com.evo_final.blackjack.game_logic.adt.PossibleActions.CanPlaceBet
import com.evo_final.blackjack.game_logic.adt.{PlayerDecision, PossibleActions}

case class Game(
  connectedClients: Map[PlayerId, Option[Amount]],
  pendingClients: Set[PlayerId],
  roundOpt: Option[Round]
) {
  def startRound(): Game =
    copy(roundOpt = Some(Round.start(connectedClients.collect { case (id, Some(_)) => id }.toList)))

  private def resetRound(): Game = {
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

  def handleDecision(id: PlayerId, decision: PlayerDecision): (Option[Game], Option[Game]) = {
    val gameAfterDecision = for {
      round              <- roundOpt
      roundAfterDecision <- round.runDecision(id, decision)
    } yield copy(roundOpt = Some(roundAfterDecision))

    val resetGame = gameAfterDecision.filter(_.roundEnded).map(_.resetRound())
    (gameAfterDecision, resetGame)
  }

  def getWinningAmounts: Option[Map[PlayerId, Amount]] = {
    roundOpt.map { round =>
      round.calculateWinningCoefficients
        .map {
          case (id, coeff) =>
            for {
              betOpt <- connectedClients.get(id)
              bet    <- betOpt
            } yield id -> bet * coeff
        }
        .collect { case Some(value) => value }
        .toMap
    }
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

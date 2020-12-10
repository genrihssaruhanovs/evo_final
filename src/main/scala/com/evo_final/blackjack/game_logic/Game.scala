package com.evo_final.blackjack.game_logic

import cats.implicits._
import com.evo_final.blackjack._

case class Game(
  connectedClients: Map[PlayerId, Amount],
  pendingClients: List[PlayerId],
  roundOpt: Option[Round]
) {
  def startRound(): Game =
    copy(roundOpt = Some(Round.start(connectedClients.filter { case (_, amount) => amount > 0 })))
  def endRound(): (Game, Map[PlayerId, Amount]) = {
    roundOpt match {
      case Some(round) =>
        (
          Game(
            connectedClients
              .map { case (id, _) => id -> BigDecimal(0) }
              .combine(pendingClients.map(_ -> BigDecimal(0)).toMap),
            List(),
            None
          ),
          round.calculateWinnings
        )
      case None => (this, Map()) // should never happen actually, rethink this method
    }
  }

  def newPlayer(id: PlayerId): Game = {
    roundOpt match {
      case Some(_) => copy(pendingClients = pendingClients :+ id)
      case None    => copy(connectedClients = connectedClients + (id -> BigDecimal(0)))
    }
  }

  def playerBet(id: PlayerId, bet: Amount): Option[Game] =
    connectedClients.get(id).filter(_ == 0).map(_ => copy(connectedClients + (id -> bet)))

  def playerDecision(id: PlayerId, decision: PlayerDecision): Option[Game] = {
    for {
      round            <- roundOpt
      roundAfterAction <- round.runDecision(id, decision)
    } yield copy(roundOpt = Some(roundAfterAction))
  }

}

object Game {
  def start: Game = Game(Map(), List(), None)
}

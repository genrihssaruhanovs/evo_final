package com.evo_final.blackjack.server

import cats.effect.{ContextShift, IO, Timer}
import com.evo_final.blackjack.{Amount, PlayerId}
import com.evo_final.blackjack.game_logic.Game
import com.evo_final.blackjack.server.FromClient._
import com.evo_final.blackjack.server.output.ToClient.{Message, Result, RoundState}
import fs2.concurrent.Queue
import cats.implicits._
import com.evo_final.blackjack.server.output.{CurrentPlayer, Dealer, OtherPlayer, ToClient}
import scala.concurrent.duration.DurationInt

case class ServerState(connectedClients: Map[PlayerId, Queue[IO, ToClient]], game: Game)(implicit
  cs: ContextShift[IO],
  timer: Timer[IO]
) {
  def newConnection(id: PlayerId, queue: Queue[IO, ToClient]): ServerState = {
    ServerState(connectedClients + (id -> queue), game.newPlayer(id))
  }

  def process(id: PlayerId, fromClient: FromClient): (IO[Option[ToClient]], ServerState) = {
    fromClient match {
      case Action(decision) =>
        game.handleDecision(id, decision) match {
          case Some(updGame) =>
            val newState = copy(game = updGame)
            if (updGame.roundEnded) {
              val resetGame = updGame.resetRound()
              val resetState = newState.copy(game = resetGame)
              val task =
                newState.updateAll() *>
                  newState.paybackTime() *>
                  resetState.resetGame().as(None)

              (task, resetState)
            } else {
              val task = newState.updateAll().as(None)
              (task, newState)
            }
          case None =>
            (IO(Some(Message("Decision couldn't be executed"))), this) //TODO make ADT
        }

      case Bet(amount) =>
        if (amount != 0) {
          game.playerBet(id, amount) match {
            case Some(updGame) =>
              val newState = copy(game = updGame)
              val task =
                if (updGame.allBetsPlaced) IO(Some(Message("Bet accepted"))) <* newState.updateAll()
                else IO(Some(Message("Bet accepted, wait till others place their bet")))
              (task, newState)
            case None => (IO(Some(Message("Bet placement failure"))), this)
          }
        } else {
          (IO(Some(Message("Bet cannot be 0"))), this)
        }
      case _ => (IO(Some(Message("Wrong request"))), this)
    }
  }

  def updateAll(): IO[Unit] = {
    connectedClients
      .map {
        case (id, queue) => queue.enqueue1(roundState(id))
      }
      .toVector
      .parSequence
      .void
  }
  def resetGame(): IO[Unit] = {
    IO.sleep(5.seconds) *> updateAll()
  }

  def paybackTime(): IO[Unit] = {
    game.getWinnings match {
      case Some(winnings) =>
        connectedClients
          .map {
            case (id, queue) =>
              val wonAmount: Amount = winnings.getOrElse(id, 0)

              val message =
                (if (wonAmount == 0) "You lost." else s"You won $wonAmount.") + "Starting new round"
              queue.enqueue1(
                Result(
                  message,
                  wonAmount
                )
              )
          }
          .toVector
          .parSequence
          .void
      case None => IO.unit
    }
  }
  def getCurrentPlayer(id: PlayerId): Option[CurrentPlayer] =
    for {
      round  <- game.roundOpt
      player <- round.players.get(id)
    } yield CurrentPlayer.of(player, round.dealer)

  def roundState(currentId: PlayerId): ToClient = {
    def noRound(): RoundState = {
      val otherPlayers = game.connectedClients.collect {
        case (id, _) if id != currentId => OtherPlayer(List(), 0, None)
      }.toList

      val possibleActions = game.getPossibleActions(currentId)
      val currentPlayer = CurrentPlayer(List(), 0, 0, Set(), possibleActions)

      RoundState(
        otherPlayers,
        currentPlayer,
        Dealer(List(), 0)
      )
    }

    val roundState = for {
      round  <- game.roundOpt
      player <- round.players.get(currentId)
    } yield RoundState(
      OtherPlayer.many(round.players, currentId),
      CurrentPlayer.of(player, round.dealer),
      Dealer.of(round.dealer)
    )
    roundState match {
      case Some(v) => v
      case None    => noRound()
    }
  }
}
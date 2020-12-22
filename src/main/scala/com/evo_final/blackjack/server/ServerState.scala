package com.evo_final.blackjack.server

import java.time.LocalTime

import cats.effect.{ContextShift, IO, Timer}
import com.evo_final.blackjack.{Amount, PlayerId}
import com.evo_final.blackjack.game_logic.Game
import com.evo_final.blackjack.server.FromClient._
import com.evo_final.blackjack.server.output.ToClient.{Balance, Message, RoundState}
import fs2.concurrent.Queue
import cats.implicits._
import com.evo_final.blackjack.game_logic.adt.PlayerDecision
import com.evo_final.blackjack.server.output.MessageToClient._
import com.evo_final.blackjack.server.output.{CurrentPlayer, Dealer, MessageToClient, OtherPlayer, ToClient}

import scala.concurrent.duration.DurationInt

case class Client(queue: Queue[IO, ToClient], balance: Amount)

case class ServerState(
  connectedClients: Map[PlayerId, Client],
  game: Game,
  pendingDisconnection: Set[PlayerId],
  timeoutAtOpt: Option[LocalTime]
)(implicit
  cs: ContextShift[IO],
  timer: Timer[IO]
) {
  def newConnection(id: PlayerId, queue: Queue[IO, ToClient]): ServerState = {
    copy(connectedClients = connectedClients + (id -> Client(queue, 0)), game = game.newPlayer(id))
  }

  def process(id: PlayerId, fromClient: FromClient): (IO[Option[ToClient]], ServerState) = {
    fromClient match {
      case Action(decision) =>
        processDecision(id, decision)
      case Bet(amount) =>
        processBet(id, amount)
      case TopUp(amount) =>
        processTopUp(id, amount)
      case _ => (messageOut(RequestFailure), this)
    }
  }

  def processDecision(id: PlayerId, decision: PlayerDecision): (IO[Option[ToClient]], ServerState) = {
    val paidForActionClient = for {
      client <- connectedClients.get(id)
      round  <- game.roundOpt
      _      <- round.players.get(id)
      bet    <- game.connectedClients(id)
      updClient = client.copy(balance = client.balance - bet * decision.costCoefficient)
    } yield updClient

    paidForActionClient match {
      case Some(client) if client.balance >= 0 =>
        val (gameAfterDecisionOpt, gameAfterResetOpt) = game.handleDecision(id, decision)

        gameAfterDecisionOpt match {
          case Some(gameAfterDecision) =>
            val afterDecisionState =
              copy(
                game = gameAfterDecision,
                connectedClients = connectedClients + (id -> client),
                timeoutAtOpt = Some(LocalTime.now().plusSeconds(10))
              )
            gameAfterResetOpt match {
              case Some(gameAfterReset) =>
                val resetState = ServerState(
                  afterDecisionState.updateBalance(),
                  removeDisconnectedFromGame(gameAfterReset),
                  Set(),
                  None
                )
                (
                  afterDecisionState.updateAllClients() *>
                    resetState.updateClientBalance() *>
                    resetState.resetGame().as(None),
                  resetState
                )
              case None =>
                (
                  afterDecisionState.updateAllClients() *> successOut(Balance(client.balance, MessageUnit)),
                  afterDecisionState
                )
            }
          case None =>
            (messageOut(DecisionFailure), this)
        }
      case Some(_) => (messageOut(InsufficientBalance), this)
      case None    => (messageOut(InternalFailure), this)
    }
  }
  def removeDisconnectedFromGame(game: Game): Game = {
    game.copy(connectedClients = game.connectedClients.filter {
      case (id, _) => !pendingDisconnection.contains(id)
    })
  }

  def processBet(id: PlayerId, amount: Amount): (IO[Option[ToClient]], ServerState) = {

    if (amount != 0) {
      connectedClients.get(id) match {
        case Some(client) =>
          val updatedBalanceClient = client.copy(balance = client.balance - amount)

          if (updatedBalanceClient.balance >= 0) {
            game.playerBet(id, amount) match {
              case Some(updGame) =>
                val timeout =
                  if (updGame.allBetsPlaced) Some(LocalTime.now().plusSeconds(10))
                  else if (timeoutAtOpt.isEmpty) Some(LocalTime.now().plusSeconds(15))
                  else timeoutAtOpt
                val newState = {
                  copy(
                    game = updGame,
                    connectedClients = connectedClients + (id -> updatedBalanceClient),
                    timeoutAtOpt = timeout
                  )
                }
                val task =
                  if (updGame.allBetsPlaced) {
                    successOut(Balance(updatedBalanceClient.balance, BetAccepted)) <* newState
                      .updateAllClients()

                  } else
                    successOut(Balance(updatedBalanceClient.balance, BetAcceptedWait)) <* newState
                      .updateSingleClient(id)

                (task, newState)
              case None => (messageOut(BetFailure), this)
            }
          } else {
            (messageOut(InsufficientBalance), this)
          }
        case None => (messageOut(InternalFailure), this)
      }
    } else {
      (messageOut(BetIsNull), this)
    }
  }

  def successOut(toClient: ToClient): IO[Option[ToClient]] = {
    IO(Some(toClient))
  }

  def messageOut(message: MessageToClient): IO[Option[ToClient]] = {
    successOut(Message(message))
  }
  def processTopUp(id: PlayerId, amount: Amount): (IO[Option[ToClient]], ServerState) = {
    if (amount != 0) {
      connectedClients.get(id) match {
        case Some(client) =>
          val newBalance = client.balance + amount
          (
            successOut(Balance(newBalance, TopUpSuccess)),
            copy(connectedClients = connectedClients + (id -> client.copy(balance = newBalance)))
          )
        case None => (messageOut(TopUpFailure), this)
      }
    } else {
      (messageOut(TopUpIsNull), this)
    }
  }
  def updateClientBalance(): IO[Unit] = {
    connectedClients
      .map {
        case (_, client) => client.queue.enqueue1(Balance(client.balance, MessageUnit))
      }
      .toVector
      .parSequence
      .void
  }

  def updateAllClients(): IO[Unit] = {
    connectedClients
      .map {
        case (id, client) => client.queue.enqueue1(roundState(id))
      }
      .toVector
      .parSequence
      .void
  }

  def updateSingleClient(id: PlayerId): IO[Unit] = {
    connectedClients.get(id) match {
      case Some(client) => client.queue.enqueue1(roundState(id))
      case None         => IO(())
    }
  }

  def resetGame(): IO[Unit] = {
    IO.sleep(5.seconds) *> updateAllClients()
  }

  def updateBalance(): Map[PlayerId, Client] = {
    val balanceRecalculatedClients = game.getWinningAmounts match {
      case Some(winnings) =>
        connectedClients
          .map {
            case (id, client) =>
              val wonAmount: Amount = winnings.getOrElse(id, 0)
              id -> client.copy(balance = client.balance + wonAmount)
          }
      case None => connectedClients
    }

    balanceRecalculatedClients.filter {
      case (id, _) => !pendingDisconnection.contains(id)
    }
  }

  def endConnection(id: PlayerId): ServerState = {
    game.roundOpt match {
      case Some(round) if round.players.contains(id) => copy(pendingDisconnection = pendingDisconnection + id)
      case _ =>
        copy(
          connectedClients = connectedClients - id,
          game = game.copy(connectedClients = game.connectedClients - id)
        )
    }
  }

  def roundState(currentId: PlayerId): ToClient = {
    def noRound(): RoundState = {
      val possibleActions = game.getPossibleActions(currentId)
      val currentPlayer = CurrentPlayer(List(), 0, 0, Set(), possibleActions)

      RoundState(
        List(),
        currentPlayer,
        Dealer(List(), 0)
      )
    }

    val roundState = for {
      betOpt <- game.connectedClients.get(currentId)
      bet    <- betOpt
      round  <- game.roundOpt
      player <- round.players.get(currentId)
    } yield RoundState(
      OtherPlayer.many(round.players, currentId),
      CurrentPlayer.of(player, round.dealer, bet),
      Dealer.of(round.dealer)
    )
    roundState match {
      case Some(v) => v
      case None    => noRound()
    }
  }

  def handleTimeouts(): (IO[Option[ToClient]], ServerState) = {
    import com.evo_final.blackjack.game_logic.adt.PlayerDecision.Stand
    import com.evo_final.blackjack.game_logic.adt.PlayerState.TurnNow
    timeoutAtOpt match {
      case Some(timeoutAt) =>
        if (timeoutAt.compareTo(LocalTime.now()) < 0) {
          game.roundOpt match {
            case Some(round) =>
              round.players.find { case (_, player) => player.states.contains(TurnNow) } match {
                case Some((id, _)) => processDecision(id, Stand)
                case None          => (IO(None), this)
              }
            case None =>
              val updatedState =
                copy(game = game.startRound(), timeoutAtOpt = Some(LocalTime.now().plusSeconds(10)))
              (updatedState.updateAllClients() *> updatedState.timeoutNotification().as(None), updatedState)
          }
        } else (IO(None), this)
      case None => (IO(None), this)
    }
  }

  def timeoutNotification(): IO[Unit] = {

    connectedClients
      .collect {
        case (id, client) if game.pendingClients.contains(id) =>
          client.queue.enqueue1(Message(BetTimeout))
      }
      .toVector
      .parSequence
      .void
  }
}

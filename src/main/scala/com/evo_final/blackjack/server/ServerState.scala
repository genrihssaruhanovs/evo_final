package com.evo_final.blackjack.server

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
  pendingDisconnection: Set[PlayerId]
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
              copy(game = gameAfterDecision, connectedClients = connectedClients + (id -> client))
            gameAfterResetOpt match {
              case Some(gameAfterReset) =>
                val resetState = ServerState(
                  afterDecisionState.updateBalance(),
                  gameAfterReset,
                  Set()
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

  def processBet(id: PlayerId, amount: Amount): (IO[Option[ToClient]], ServerState) = {

    if (amount != 0) {
      connectedClients.get(id) match {
        case Some(client) =>
          val updatedBalanceClient = client.copy(balance = client.balance - amount)

          if (updatedBalanceClient.balance >= 0) {
            game.playerBet(id, amount) match {
              case Some(updGame) =>
                val newState =
                  copy(game = updGame, connectedClients = connectedClients + (id -> updatedBalanceClient))
                val task =
                  if (updGame.allBetsPlaced) {
                    successOut(Balance(updatedBalanceClient.balance, BetAccepted)) <* newState
                      .updateAllClients()

                  } else
                    successOut(Balance(updatedBalanceClient.balance, BetAcceptedWait))
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
      case _                                         => copy(connectedClients = connectedClients - id)
    }
  }
//  def paybackTime(): IO[Unit] = {
//    game.getWinningAmounts match {
//      case Some(winnings) =>
//        connectedClients
//          .map {
//            case (id, client) =>
//              val wonAmount: Amount = winnings.getOrElse(id, 0)
//
//              val message =
//                (if (wonAmount == 0) "You lost." else s"You won $wonAmount.") + "Starting new round"
//              client.queue.enqueue1(
//                Balance(
//                  message,
//                  wonAmount
//                )
//              )
//          }
//          .toVector
//          .parSequence
//          .void
//      case None => IO.unit
//    }
//  }
//  def getCurrentPlayer(id: PlayerId): Option[CurrentPlayer] =
//    for {
//      betOpt <- game.connectedClients.get(id)
//      bet    <- betOpt
//      round  <- game.roundOpt
//      player <- round.players.get(id)
//    } yield CurrentPlayer.of(player, round.dealer, bet)

//  def handleTimeouts(): IO[ServerState] = {
//    if (lastActionAt + TimeUnit.SECONDS.toMillis(5) > System.currentTimeMillis()) {
//      val gameUpdOpt = game.roundOpt match {
//        case Some(round) =>
//          for {
//            (id, _)      <- round.players.find { case (_, player) => player.states.contains(TurnNow) }
//            finishedMove <- game.handleDecision(id, Stand)
//            //        } round.players.find { case (_, player) => player.states.contains(TurnNow) } match {
//            //          case Some((id, _)) => game.handleDecision(id, Stand)
//            //          case None          => game
//          } yield finishedMove
//
//        case None => Some(game.startRound())
//      }
//      gameUpdOpt match {
//        case Some(gameUpd) =>
//          val updatedState = copy(game = gameUpd, lastActionAt = System.currentTimeMillis())
//          updatedState.updateAll() *> IO(updatedState)
//        case None => IO(this)
//      }
//    } else IO(this)
//  }

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
}

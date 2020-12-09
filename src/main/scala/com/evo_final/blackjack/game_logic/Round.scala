package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack._

case class Round(
  players: Map[PlayerId, Player],
  dealer: Dealer,
  deck: GameDeck,
  inProcess: Boolean
) {
  def calculateWinnings: Map[PlayerId, Amount] = {
    players.map {
      case (id, player) => id -> player.evaluate(dealer.hand)
    }
  }

  def addPlayer(playerId: PlayerId, roundInProgress: Boolean): Round = {
    val newPlayer =
      Player.create(!roundInProgress) // if round in progress - player not in the game this round
    copy(players = players + (playerId -> newPlayer))
  }

  def prepare(playerIds: List[PlayerId]): Round = {
    Round(
      setPlayers(playerIds),
      Dealer(DealerHand(List())),
      GameDeck.empty,
      inProcess = false
    )
  }

//  def start: Round = {
//    def servePlayers(unservedPlayers: List[(PlayerId, Player)], servedPlayers: Map[PlayerId, Player], currentDeck: GameDeck): Map[PlayerId, Player] = {
//      unservedPlayers match {
//        case x :: xs =>
//          val (id, player) = x
//          for {
//            served <- player.initialServe(currentDeck)
//            (servedPlayer, newDeck) = served
//            served
//          }
//
//        case Nil => servedPlayers
//      }
//    }
//    for {
//      playerM <- playersь
//      (id, player)
//    }
//    val servedPlayers
//  }

//    Round(
//      resetPlayers,
//      Dealer(DealerHand(List())),
//      GameDeck.of(4),
//      inProcess = true
//
  def setPlayers(playerIds: List[PlayerId]): Map[PlayerId, Player] =
    playerIds.map(id => id -> Player.create(true)).toMap

  def placeBet(id: PlayerId, bet: Amount): Either[ErrorMessage, Round] =
    for {
      player        <- findPlayer(id)
      updatedPlayer <- player.placeBet(bet)
    } yield copy(players = players + (id -> updatedPlayer))

  def doAction(id: PlayerId, decision: PlayerDecision): Either[ErrorMessage, Round] =
    for {
      player <- findPlayer(id)
      result <- player.makeDecision(decision, deck)
      (updatedPlayer, newDeck) = result
    } yield copy(players = players + (id -> updatedPlayer), deck = newDeck)

  def findPlayer(id: PlayerId): Either[ErrorMessage, Player] =
    players.get(id) match {
      case Some(player) => Right(player)
      case None         => Left("Player does not exist")
    }
}

object Round {}

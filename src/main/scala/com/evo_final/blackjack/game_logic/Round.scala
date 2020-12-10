package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack._

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
      result <- player.makeDecision(decision, deck)
      (updatedPlayer, newDeck) = result
    } yield copy(players = players + (id -> updatedPlayer), deck = newDeck)
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

    Round(servedPlayers, servedDealer, resultDeck)
  }

}

package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack._

case class Round(
  players: List[Player],
  dealer: Dealer
) {
  def calculateWinnings: Map[PlayerId, Amount] = {
    players.map(x => x.id -> Round.evaluate(x, dealer)).toMap
  }
}

object Round {
  def evaluate(player: Player, dealer: Dealer): Amount = {
    val playerBlackJack = player.currentScore == 21 && player.hand.cards.size == 2
    val dealerBlackJack = dealer.currentScore == 21 && dealer.hand.cards.size == 2

    if (playerBlackJack && dealerBlackJack) player.bet
    else if (playerBlackJack) player.bet * 2.5
    else if (dealerBlackJack) 0 // Insurance logic implementation
    else if (player.currentScore == dealer.currentScore) player.bet
    else if (player.currentScore > dealer.currentScore) player.bet * 2
    else 0
  }
}

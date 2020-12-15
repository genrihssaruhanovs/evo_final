package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic._

case class CurrentPlayer(
  cards: List[Card],
  score: Int,
  bet: Amount,
  states: Set[PlayerState],
  possibleActions: Set[PossibleActions]
)

object CurrentPlayer {
  def of(player: Player, dealer: Dealer): CurrentPlayer = {
    CurrentPlayer(
      player.hand.cards,
      player.hand.score,
      player.bet,
      player.states,
      player.getPossibleActions(dealer)
    )
  }
}

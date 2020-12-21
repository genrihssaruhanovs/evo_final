package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic._
import com.evo_final.blackjack.game_logic.adt.{PlayerState, PossibleActions}

case class CurrentPlayer(
  cards: List[Card],
  score: Int,
  bet: Amount,
  states: Set[PlayerState],
  possibleActions: Set[PossibleActions]
)

object CurrentPlayer {
  def of(player: Player, dealer: Dealer, bet: Amount): CurrentPlayer = {
    CurrentPlayer(
      player.hand.cards,
      player.hand.score,
      bet,
      player.states,
      player.getPossibleActions(dealer)
    )
  }
}

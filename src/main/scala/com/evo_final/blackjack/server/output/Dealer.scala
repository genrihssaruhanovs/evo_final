package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.cards.Card

case class Dealer(
  cards: List[Card],
  score: Int,
)
object Dealer {
  def of(dealer: com.evo_final.blackjack.game_logic.Dealer): Dealer = {
    Dealer(dealer.hand.cards, dealer.hand.score)
  }
}

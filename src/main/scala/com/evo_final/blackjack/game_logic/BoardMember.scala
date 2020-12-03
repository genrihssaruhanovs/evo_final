package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.adt.PlayerState
import com.evo_final.blackjack.{Amount, PlayerId}

sealed trait BoardMember {
  def hand: PlayerHand
  def currentScore: Int = hand.evaluate
}

case class Player(
  id: PlayerId,
  hand: PlayerHand,
  states: List[PlayerState],
  bet: Amount,
) extends BoardMember {}

case class Dealer(hand: PlayerHand) extends BoardMember {
  def play(dealer: Dealer, deck: GameDeck): Option[(Dealer, GameDeck)] = {
    if (dealer.currentScore > 17) Some((dealer, deck))
    else {
      for {
        served <- deck.serve
        (card, newDeck) = served
        newHand <- PlayerHand.of(card :: hand.cards)
        result  <- play(Dealer(newHand), newDeck)
      } yield result
    }
  }
}

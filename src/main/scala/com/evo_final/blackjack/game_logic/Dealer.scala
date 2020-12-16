package com.evo_final.blackjack.game_logic

import scala.annotation.tailrec

case class Dealer(hand: Hand) {
  def play(deck: GameDeck): (Dealer, GameDeck) = {

    @tailrec
    def getCards(currentHand: Hand, deck: GameDeck): (Hand, GameDeck) = {
      if (currentHand.score > 17) (currentHand, deck)
      else {
        val (newHand, newDeck) = currentHand.getServed(1, deck)
        getCards(newHand, newDeck)
      }
    }

    val (resultHand, resultDeck) = getCards(hand, deck)
    (Dealer(resultHand), resultDeck)
  }
}

object Dealer {
  def of(deck: GameDeck): (Dealer, GameDeck) = {
    val (servedCards, newDeck) = deck.serveN(1)
    (Dealer(Hand(servedCards)), newDeck)
  }
}

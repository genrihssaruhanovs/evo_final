package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.game_logic.PlayerDecision._
import com.evo_final.blackjack.game_logic.PlayerState.{
  BetPlaced,
  Insured,
  IsInGame,
  Surrendered,
  TurnDone,
  TurnNow
}
import com.evo_final.blackjack.{Amount, ErrorMessage}

import scala.annotation.tailrec

sealed trait BoardMember {
  def hand: Hand
}

case class Player(
  hand: Hand,
  states: Set[PlayerState],
  bet: Amount,
) extends BoardMember {

  def evaluate(dealerHand: Hand): Amount =
    if (hand.isBlackJack && dealerHand.isBlackJack)
      if (states.contains(Insured)) bet * 2 else bet
    else if (hand.isBlackJack) bet * 2.5
    else if (hand.isBlackJack)
      if (states.contains(Insured)) bet else 0
    else if (hand.score == dealerHand.score) bet
    else if (hand.score > dealerHand.score) bet * 2
    else 0

  def makeDecision(decision: PlayerDecision, deck: GameDeck): Option[(Player, GameDeck)] = {
    if (states.contains(TurnDone)) None
    else if (!states.contains(TurnNow)) None
    else {
      decision match {
        case Hit =>
          val (newHand, newDeck) = hand.getServed(1, deck)
          val newStates = if (newHand.score > 21) statesEndTurn else states
          Some(copy(hand = newHand, states = newStates), newDeck)

        case Stand => Some(copy(states = statesEndTurn), deck)

        case DoubleDown =>
          val (newHand, newDeck) = hand.getServed(1, deck)
          Some((copy(hand = newHand, states = statesEndTurn, bet = bet * 2), newDeck))

        case Surrender => Some(copy(states = statesEndTurn + Surrendered), deck)

        case Split => ???

        case Insurance => Some(copy(states = statesEndTurn + Insured), deck)

      }
    }
  }

  def statesEndTurn: Set[PlayerState] = states + TurnDone - TurnNow

//  def initialServe(deck: GameDeck): (Player, GameDeck) = {
//    val (newHand, newDeck) = hand.getServed(2, deck)
//    (copy(hand = newHand), newDeck)
//  }

}

object Player {
  def of(deck: GameDeck, bet: Amount): (Player, GameDeck) = {
    val (servedCards, newDeck) = deck.serveN(2)
    (Player(Hand(servedCards), Set(), bet), newDeck)
  }
}

case class Dealer(hand: Hand) extends BoardMember {
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

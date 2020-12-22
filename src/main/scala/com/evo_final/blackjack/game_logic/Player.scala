package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Rank.Ace
import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.game_logic.adt.PlayerDecision._
import com.evo_final.blackjack.game_logic.adt.PlayerState._
import com.evo_final.blackjack.game_logic.adt.HandResult.Won
import com.evo_final.blackjack.game_logic.adt.PossibleActions._
import com.evo_final.blackjack.game_logic.adt.{PlayerDecision, PlayerState, PossibleActions}

import scala.language.implicitConversions

case class Player(
  hand: Hand,
  states: Set[PlayerState]
) {

  def evaluate(dealerHand: Hand): Amount = {
    implicit def bool2int(b: Boolean): BigDecimal = if (b) 1 else 0

    if (states.contains(Surrendered))
      0.5
    else {

      val handResult = hand.handResult(dealerHand)

      handResult.coefficient +
        (this.hand.isBlackJack && handResult == Won) * 0.5 +
        (states.contains(Insured) && dealerHand.isBlackJack) * 1.5 +
        (states.contains(DoubleDowned) && handResult == Won) * 2
    }
  }

  def makeDecision(decision: PlayerDecision, deck: GameDeck): (Player, GameDeck) = {
    decision match {
      case Hit =>
        val (newHand, newDeck) = hand.getServed(1, deck)
        val newStates = if (newHand.score > 21) statesEndTurn else states
        (copy(hand = newHand, states = newStates), newDeck)

      case Stand => (copy(states = statesEndTurn), deck)

      case DoubleDown =>
        val (newHand, newDeck) = hand.getServed(1, deck)
        (copy(hand = newHand, states = statesEndTurn + DoubleDowned), newDeck)

      case Surrender => (copy(states = statesEndTurn + Surrendered), deck)

      case Split => (this, deck) //TODO

      case Insurance => (copy(states = states + Insured), deck)
    }
  }

  private def statesEndTurn: Set[PlayerState] = states + TurnDone - TurnNow

  def getPossibleActions(dealer: Dealer): Set[PossibleActions] = {
    def getInitialActions(states: Set[PlayerState]): Set[PossibleActions] = {

      def canDoubleDown: Set[PossibleActions] =
        if (!states.contains(DoubleDowned)) Set(CanDoubleDown) else Set()
      def canSurrender: Set[PossibleActions] =
        if (!states.contains(Surrendered)) Set(CanSurrender) else Set()
      def canInsure: Set[PossibleActions] =
        if (!states.contains(Insured) && dealer.hand.cards.head.rank == Ace) Set(CanInsure)
        else Set()
      def canSplit: Set[PossibleActions] =
        if (hand.cards.head.rank == hand.cards.last.rank) Set(CanSplit) else Set()

      canDoubleDown ++ canSurrender ++ canInsure ++ canSplit
    }

    if (states.contains(TurnNow)) {
      val defaultActions = Set(CanStand, CanHit)
      val initialActions = if (hand.cards.size == 2) getInitialActions(states) else Set()

      defaultActions ++ initialActions
    } else Set()

  }
}

object Player {
  def of(deck: GameDeck): (Player, GameDeck) = {
    val (servedCards, newDeck) = deck.serveN(2)
    (Player(Hand(servedCards), Set()), newDeck)
  }
}

package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Rank.Ace
import com.evo_final.blackjack.{Amount, PlayerId}
import com.evo_final.blackjack.game_logic.PlayerDecision._
import com.evo_final.blackjack.game_logic.PlayerState._
import com.evo_final.blackjack.game_logic.HandResult.Won
import com.evo_final.blackjack.game_logic.PossibleActions._

case class Player(
  hand: Hand,
  states: Set[PlayerState],
  bet: Amount,
) {

  def evaluate(dealerHand: Hand): Amount = {
    implicit def bool2int(b: Boolean): BigDecimal = if (b) 1 else 0

    val blackJackCoefficient: Amount =
      if (this.hand.isBlackJack == dealerHand.isBlackJack) 0
      else if (this.hand.isBlackJack) 0.5
      else -1

    val handResult = hand.handResult(dealerHand)

    bet * (
      handResult.coefficient +
        blackJackCoefficient +
        (states.contains(Insured) && dealerHand.isBlackJack) +
        (states.contains(DoubleDowned) && handResult == Won)
    ).max(0)
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

      case Split => ???

      case Insurance => (copy(states = statesEndTurn + Insured), deck)
    }
  }

  def statesEndTurn: Set[PlayerState] = states + TurnDone - TurnNow

  def getPossibleActions(dealer: Dealer): Set[PossibleActions] = {
    def getInitialActions(states: Set[PlayerState]): Set[PossibleActions] = {

      def canDoubleDown: Set[PossibleActions] =
        if (!states.contains(DoubleDowned)) Set(CanDoubleDown) else Set()
      def canSurrender: Set[PossibleActions] =
        if (!states.contains(Surrendered)) Set(CanSurrender) else Set()
      def canInsure: Set[PossibleActions] =
        if (!states.contains(Surrendered) && dealer.hand.cards.head.rank == Ace) Set(CanSurrender)
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

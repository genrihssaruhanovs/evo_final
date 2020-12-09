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

sealed trait BoardMember[H <: Hand[H]] {
  def hand: Hand[H]
}

case class Player(
  hand: PlayerHand,
  states: Set[PlayerState],
  bet: Amount,
) extends BoardMember[PlayerHand] {

  def reset: Player = Player.create(true)

  def evaluate(dealerHand: DealerHand): Amount =
    if (hand.isBlackJack && dealerHand.isBlackJack)
      if (states.contains(Insured)) bet * 2 else bet
    else if (hand.isBlackJack) bet * 2.5
    else if (hand.isBlackJack)
      if (states.contains(Insured)) bet else 0
    else if (hand.score == dealerHand.score) bet
    else if (hand.score > dealerHand.score) bet * 2
    else 0

  def placeBet(bet: Amount): Either[ErrorMessage, Player] =
    if (states.contains(BetPlaced)) Left("Bet is already placed")
    else if (!states.contains(IsInGame)) Left("Cannot place bet - player not in game")
    else Right(copy(states = states + BetPlaced, bet = bet))

  def makeDecision(decision: PlayerDecision, deck: GameDeck): Either[ErrorMessage, (Player, GameDeck)] = {
    if (states.contains(TurnDone)) Left("Turn already done")
    else if (!states.contains(TurnNow)) Left("Now is not your turn")
    else {
      decision match {
        case Hit =>
          for {
            served <- deck.serveN(1)
            (cards, newDeck) = served
            newHand <- PlayerHand.of(hand.cards ++ cards)
            newStates = if (newHand.score > 21) endTurn else states
          } yield (copy(hand = newHand, states = newStates), newDeck)

        case Stand => Right(copy(states = endTurn), deck)

        case DoubleDown =>
          for {
            served <- deck.serveN(1)
            (cards, newDeck) = served
            newHand <- PlayerHand.of(hand.cards ++ cards)
          } yield (copy(hand = newHand, states = endTurn, bet = bet * 2), newDeck)

        case Surrender => Right(copy(states = endTurn + Surrendered), deck)

        case Split => ???

        case Insurance => Right(copy(states = endTurn + Insured), deck)

      }
    }
  }

  def endTurn: Set[PlayerState] = states + TurnDone - TurnNow

  def initialServe(deck: GameDeck): Either[ErrorMessage, (Player, GameDeck)] = {
    for {
      served <- deck.serveN(2)
      (cards, newDeck) = served
      newHand <- PlayerHand.of(cards)
    } yield (copy(hand = newHand), newDeck)
  }

}

object Player {
  def create(isInGame: Boolean): Player = {
    val states: Set[PlayerState] = if (isInGame) Set(IsInGame) else Set()
    Player(PlayerHand.empty, states, 0)
  }
}

case class Dealer(hand: DealerHand) extends BoardMember[DealerHand] {
  def play(deck: GameDeck): Either[ErrorMessage, (Dealer, GameDeck)] = {
    def getCards(currentHand: DealerHand, deck: GameDeck): Either[ErrorMessage, (DealerHand, GameDeck)] = {
      if (currentHand.score > 17) Right(currentHand, deck)
      else {
        for {
          served <- deck.serveN(1)
          (card, newDeck) = served
          newHand <- DealerHand.of(card ++ currentHand.cards)
          result  <- getCards(newHand, newDeck)
        } yield result
      }
    }

    getCards(hand, deck).map { case (newHand, newDeck) => (Dealer(newHand), newDeck) }

  }
  def initialServe(deck: GameDeck): Either[ErrorMessage, (Dealer, GameDeck)] = {
    for {
      served <- deck.serveN(1)
      (card, newDeck) = served
      newHand <- DealerHand.of(card ++ hand.cards)
    } yield (Dealer(newHand), newDeck)
  }
}

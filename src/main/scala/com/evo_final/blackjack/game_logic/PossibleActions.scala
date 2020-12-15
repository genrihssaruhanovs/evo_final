package com.evo_final.blackjack.game_logic

sealed trait PossibleActions

object PossibleActions {
  case object CanPlaceBet extends PossibleActions
  case object CanHit extends PossibleActions
  case object CanStand extends PossibleActions
  case object CanSurrender extends PossibleActions
  case object CanInsure extends PossibleActions
  case object CanSplit extends PossibleActions
  case object CanDoubleDown extends PossibleActions
}

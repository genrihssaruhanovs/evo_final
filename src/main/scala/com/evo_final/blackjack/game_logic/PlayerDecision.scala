package com.evo_final.blackjack.game_logic

sealed trait PlayerDecision

object PlayerDecision {

  case object Hit extends PlayerDecision

  case object Stand extends PlayerDecision

  case object DoubleDown extends PlayerDecision

  case object Surrender extends PlayerDecision

  case object Split extends PlayerDecision

  case object Insurance extends PlayerDecision

}

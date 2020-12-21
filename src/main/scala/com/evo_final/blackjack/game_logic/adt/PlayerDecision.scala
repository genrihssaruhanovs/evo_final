package com.evo_final.blackjack.game_logic.adt

import com.evo_final.blackjack.Amount

sealed trait PlayerDecision {
  def costCoefficient: Amount
}

object PlayerDecision {

  case object Hit extends PlayerDecision {
    override def costCoefficient: Amount = 0
  }

  case object Stand extends PlayerDecision {
    override def costCoefficient: Amount = 0
  }

  case object DoubleDown extends PlayerDecision {
    override def costCoefficient: Amount = 1
  }

  case object Surrender extends PlayerDecision {
    override def costCoefficient: Amount = 0
  }

  case object Split extends PlayerDecision {
    override def costCoefficient: Amount = 1
  }

  case object Insurance extends PlayerDecision {
    override def costCoefficient: Amount = 0.5
  }

}

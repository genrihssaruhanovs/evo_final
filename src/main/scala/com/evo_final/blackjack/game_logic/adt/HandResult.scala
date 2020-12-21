package com.evo_final.blackjack.game_logic.adt

sealed trait HandResult {
  def coefficient: Int
}

object HandResult {
  case object Won extends HandResult {
    val coefficient = 2
  }
  case object Tie extends HandResult {
    val coefficient = 1
  }
  case object Lost extends HandResult {
    val coefficient = 0
  }
}

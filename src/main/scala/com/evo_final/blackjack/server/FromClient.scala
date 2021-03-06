package com.evo_final.blackjack.server

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.game_logic.adt.PlayerDecision

sealed trait FromClient

object FromClient {
  final case class Bet(amount: Amount) extends FromClient
  final case class Action(decision: PlayerDecision) extends FromClient
  final case class TopUp(amount: Amount) extends FromClient
}

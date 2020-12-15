package com.evo_final.blackjack.server

import com.evo_final.blackjack.{Amount, PlayerId}
import com.evo_final.blackjack.game_logic.PlayerDecision
import io.circe.generic.JsonCodec
import io.circe.generic.auto._

@JsonCodec
sealed trait FromClient

object FromClient {
  final case class Bet(amount: Amount) extends FromClient
  final case class Action(decision: PlayerDecision) extends FromClient
//  @JsonCodec final case class Connect(id: PlayerId) extends FromClient
}

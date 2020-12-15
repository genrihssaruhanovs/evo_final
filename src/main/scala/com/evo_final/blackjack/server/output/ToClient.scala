package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic.{Player, PlayerState, PossibleActions}
import io.circe.generic.JsonCodec
import io.circe.generic.auto._

@JsonCodec sealed trait ToClient

object ToClient {
  case class Communication(possibleActions: List[PossibleActions], text: String) extends ToClient
  case class Message(text: String) extends ToClient
  case class RoundState(
    otherPlayers: List[OtherPlayer],
    currentPlayer: CurrentPlayer,
    dealer: Dealer
  ) extends ToClient
  case class PlayerState(
    currentPlayer: CurrentPlayer
  )
  case class Result(
    message: String,
    amount: Amount
  ) extends ToClient
}
